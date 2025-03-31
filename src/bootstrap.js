"use strict"
// echo 'def main: io.puts "hello"' | node bootstrap.js | node | grep hello

const runtime = (() => {
  class MoaError extends Error {
    constructor(data) {
      super(data)
      this.data = data
    }
  }
  const some = v => v
  const none = null
  const tuple = (...a) => a
  const vec = (...a) => a
  const set = (...a) => new Set(a)
  const map = (...a) => new Map([...new Array(a.length / 2)].map((_, i) => [a[i*2], a[i*2+1]]))
  const log = (a, ...b) => { console.warn(a, ...b); return a }
  const assert = (cond, f) => cond || __throw(f())
  const __throw = s => { throw new MoaError(s) }
  const __catch = (f, g) => {
    try {
      return f()
    } catch (e) {
      if (e instanceof MoaError) {
        return g(e.data)
      } else {
        throw e
      }
    }
  }
  const __prop = (obj, name, ...a) => {
    if (name === "then") {
      return obj === none ? none : a[0](obj)
    } else if (name === "or") {
      return obj === none ? a[0]() : obj
    } else if (name === "at") {
      return n => obj[n]
    } else if (name.match(/^[0-9]/)) {
      return obj[name]
    } else if (typeof obj === "object" && name in obj) {
      return obj[name]
    } else if (typeof obj === "string" && name === "size") {
      return obj.length
    } else {
      throw new Error(`No ${name} of ${typeof obj}`)
    }
  }
  const __op2 = (op2, lhs, f) =>
    op2 === "|||" ? (lhs === none ? f() : lhs) :
    op2 === "==" ? __str(lhs) == __str(f()) :
    op2 === "!=" ? __str(lhs) != __str(f()) :
    op2 === "<"  ? lhs <  f() :
    op2 === "<=" ? lhs <= f() :
    op2 === ">"  ? lhs >  f() :
    op2 === ">=" ? lhs >= f() :
    op2 === "+"  ? lhs + f() :
    op2 === "-"  ? lhs - f() :
    op2 === "*"  ? lhs * f() :
    op2 === "/"  ? lhs / f() :
    op2 === "%"  ? lhs % f() :
    (() => { throw new Error(`Unknown operator '${op2}'`)})()
  const __str = o => Array.isArray(o) ? o.map(__str).join(" ") :
    o instanceof Map ? [...o.keys()].sort().map(key => key + "::" + __str(o.get(key))).join(" ") :
    o instanceof Set ? [...o].sort().join(" ") :
    typeof o === 'object' ? Object.keys(o).sort().map(key => key + "::" + __str(o[key])).join(" ") :
    JSON.stringify(o)
}).toString().slice("() => {".length, -1).trim()

const tokenize = moa => {
  let lineno = 1
  let offset = 0
  let indent = 0
  const tokens = []
  for (const code of moa.split(/([ \n]+|#[^\n]*|[()\[\]{}]|[0-9+]+\.[0-9]+|[:.+\-*/%!=^|&?><]+|"[^"]*"|`[^`]*`|[ \n]+)/)) {
    if (code.trim() && code[0] !== "#") {
      const op1 = code === "!"
      const op2 = "+-*/%|&<>=!".includes(code[0])
      tokens.push({code, lineno, offset, indent, ...(op1 && {op1}), ...(op2 && {op2})})
    }
    offset += code.length
    lineno += code.split("\n").length - 1
    indent = code.includes("\n") ? code.split("\n").at(-1).length : indent
  }
  return tokens
}

const parse = tokens => {
  // this parser does not care about the priority of operators
  let pos = 0
  const until = (f, g, h) => {
    const a = []
    while (pos < tokens.length && f(tokens[pos])) {
      a.push(g(tokens[pos]))
    }
    h && h()
    return a
  }
  const untilBy = code => until(t => t.code !== code, parseUnit, _ => ++pos)
  const parseUnit = () => {
    const t = tokens[pos++]
    if (!t) {
      throw new Error()
    }
    if (t.code === ":") {
      const indent = tokens[pos].indent
      return t.lineno === tokens[pos].lineno ?
        [t, parseLine(tokens[pos])] :
        [t].concat(until(tt => tt.indent === indent, parseLine))
    }
    const link = t => {
      const nospace = pos < tokens.length && tokens[pos-1].offset + tokens[pos-1].code.length === tokens[pos].offset
      return pos >= tokens.length ? t :
        tokens[pos].code === "." ? link([tokens[pos++], t, parseUnit()]) :
        tokens[pos].code === "(" && nospace ? pos++ && link([t, ...untilBy(")")]) :
        tokens[pos].code === "[" && nospace ? pos++ && link([[{code: "."}, t, {code: "at"}], ...untilBy("]")]) :
        tokens[pos].op1 ? link([tokens[pos++], t]) :
        tokens[pos].op2 ? link([tokens[pos++], t, parseUnit()]) :
        t
    }
    return link(
      t.code === "(" ? [{parenthesis: true}, untilBy(")")[0]] :
      t.code === "[" ? [{code: "vec"}, ...untilBy("]")] :
      t)
  }
  const parseLine = (t) => {
    const a = until(tt => t.lineno === tt.lineno && !")]}".includes(tt.code), parseUnit)
    if (a.length === 0) {
      console.dir({next: t, tokens: tokens.slice(0)})
      throw new Error(`BUG a line has no elements ${pos}`)
    }
    return a.length === 1 ? a[0] : a
  }
  return until(() => true, parseLine)
}

const generate = nodes => {
  const geniif = a => a.length === 1 ? a[0] : a[0] + " ? " + a[1] + " : " + geniif(a.slice(2))
  const genif = a => `if ((${a.slice(0, -1).join(") && (")})) ${a.at(-1)}`
  const genelse = a => "else " + (a[0].code === "if" ? genif(a.slice(1).map(gen)) : gen(a[0]))
  const gendef = (a, body) => `const ${a[0]} = (${a.slice(1)}) => ${body}`
  const genclass = (name, fields) => `const ${name} = (${fields}) => ({${fields}})`
  const genenum = o => Array.isArray(o) ?
    `const ${o[0].code} = __val => ({__tag: "${o[0].code}", __val})` :
    `const ${o.code} = {__tag: "${o.code}"}`
  const genmatch = a => a.length === 0 ? `__throw("No match")` :
    a[0].length === 3 ?
    `__tag === "${a[0][0].code}" ? (${a[0][1].code} => ${genexp(a[0][1])})(__val) : ${genmatch(a.slice(1))}` :
    `__tag === "${a[0][0].code}" ? ${genexp(a[0][1])} : ${genmatch(a.slice(1))}`
  const genexp = o => Array.isArray(o) && o[0].code === ":" ? genreturn(o.slice(1).map(gen)) : gen(o)
  const genreturn = a => a.length === 1 ? a[0] : `(() => {${a.slice(0, -1).map(s => s + ";").join("")}return ${a.at(-1)}})()`
  const gen = node => {
    if (Array.isArray(node)) {
      const head = node[0]
      const tail = node.slice(1)
      if (head.code === "[") {
        return "[" + tail.map(gen) + "]"
      } else if (head.code === "{") {
        const a = tail.map(gen)
        return "(() => {" + a.slice(0, -1).join(";\n") + ";\n return " + a.at(-1) + "})()"
      } else if (head.op1) {
        return head.code + gen(tail[0])
      } else if (head.code === ".") {
        const [field, ...args] = Array.isArray(tail[1]) ? tail[1].map(gen) : [gen(tail[1])]
        return `__prop(${gen(tail[0])}, "${field}"${args.map(s => " ," + s).join("")})`
      } else if (head.op2) {
        return head.code === "=>" ? `${gen(tail[0])} => ${gen(tail[1])}` :
          "+= -= *= /= %= **= ||= &&=".split(" ").includes(head.code) ? `${tail[0].code} = __op2("${head.code.slice(0, -1)}", ${gen(tail[0])}, () => ${gen(tail[1])})` :
          `__op2("${head.code}", ${gen(tail[0])}, () => ${gen(tail[1])})`
      } else if (head.parenthesis) {
        return "(" + gen(tail[0]) + ")"
      } else {
        return head.code === ":" ? "{" + tail.map(gen).join(";\n") + "}" :
          head.code === "(" ? gen(tail) :
          head.code === "assert" ? `assert(${gen(tail[0])}, () => [${tail.slice(1).map(gen)}].join(" "))` :
          head.code === "catch" ? `__catch(() => ${gen(tail[0])}, ${gen(tail[1])})` :
          head.code === "iif" ? geniif(tail.map(gen)) :
          head.code === "if" ? genif(tail.map(gen)) :
          head.code === "else" ? genelse(tail) :
          head.code === "def" ? gendef(tail.slice(0, -1).map(node => node.code), gen(tail.at(-1))) :
          head.code === "class" ? genclass(tail[0].code, tail[1].slice(1).map(x => x[0].code)) :
          head.code === "let" ? `const ${tail[0][1].code} = ${gen(tail[0][2])}` :
          head.code === "var" ? `let ${tail[0][1].code} = ${gen(tail[0][2])}` :
          head.code === "enum" ? tail[1].slice(1).map(genenum).join("\n") :
          head.code === "match" ? `;(({__tag, __val}) => ${genmatch(tail[1].slice(1))})(${gen(tail[0])})` :
          head.code === "while" ? `while (${gen(tail[0])}) { ${gen(tail[1])} }` :
          head.code === "dec" ? "" :
          head.code === "interface" ? "" :
          gen(head) + "(" + tail.map(gen).join(", ") + ")"
      }
    } else if (node === undefined) {
      return ""
    } else {
      return node.code === "void" ? null :
        node.code === "throw" ? "__throw" :
        node.code
    }
  }
  return nodes.map(gen).join("\n")
}

const test1 = () => {
  const vm = require("node:vm")
  const assert = require('node:assert');
  const eq = (expected, moa) => {
    const tokens = tokenize(moa)
    const nodes = parse(tokens)
    const js = generate(nodes)
    const logs = []
    const context = {
      console: {
        warn: (...a) => logs.push(a.join(" "))
      }
    }
    let actual = null
    try {
      actual = new vm.Script(runtime + ";\n" + js).runInNewContext(context)
    } catch (e) {
      actual = "error: " + e.message
    }
    if (logs.length) {
      actual = "log: " + logs.join("\n")
    }
    assert.deepEqual(actual, expected, `${actual} != ${expected}\n\n# moa\n${moa}\n\n# js\n${js}\n\n# nodes\n${JSON.stringify(nodes, null, 2)}`)
    process.stderr.write(".")
  }

  // Primitive
  eq(undefined, "void")
  eq(1, "1")
  eq(1.1, "1.1")
  eq("hi", '"hi"')
  eq('"hi"', '`"hi"`')
  eq('hi\n', '"hi\\n"')
  eq(true, "true")
  eq(1, "(a => a)(1)")

  // Container
  eq(1, "some(1)")
  eq(null, "none")
  eq("a", 'tuple(1 "a").1')
  eq([1, 2], "vec(1 2)")
  eq([1, 2], "[1 2]")
  eq(new Set([1, 2]), "set(1 1 2)")
  eq(new Map([["a", 1]]), 'map("a" 1)')

  // Methods
  eq("a", '"ab"[0]')
  eq(2, '"ab".size')
  eq(1, "some(1) ||| none")
  eq(1, "none ||| some(1)")
  eq(null, "none ||| none")
  eq(null, "none.then(a => a + 1)")
  eq(2, "some(1).then(a => a + 1)")
  eq(1, "none.or(() => 1)")
  eq(2, "some(2).or(() => 1)")

  // Expression
  eq(3, "1 + 2")
  eq(7, "1 + 2 * 3")
  eq(9, "(1 + 2) * 3")
  eq(true, "1 == 1")
  eq(true, "set(1) == set(1)")
  eq(false, "set(1) == set(2)")
  eq(true, "map(1 2) == map(1 2)")
  eq(false, "map(1 2) == map(1 3)")

  // Exception
  eq("error: 1", "throw(1)")
  eq(1, "catch(1 n => n + 1)")
  eq(2, "catch(throw(1) n => n + 1)")

  // Embedded
  eq("log: 1 2", "log(1 2)")
  eq("1", "assert(true)\n1")
  eq("error: 1 2", "assert(false 1 2)\n1")

  // Branch
  eq(1, "iif(true 1 2)")
  eq(2, "iif(false throw(1) 2)")
  eq(3, "iif(false 1 false 2 3)")
  eq(2, "if false: log(1)\n2")
  eq("log: 1", "if true: log(1)\n2")
  eq("log: 2", "if false: log(1)\nelse: log(2)")
  eq("log: 2", "if false: log(1)\nelse if true: log(2)")
  eq("log: 3", "if false: log(1)\nelse if false: log(2)\nelse: log(3)")

  // Declare
  eq(1, "let a = 1\na")
  eq(3, "var a = 1\na += 2\na")
  eq(1, "def f: return 1\nf()")
  eq([], "def f: return []\nf()")
  eq(1, "def f:\n  if true: return 1\n  return 2\nf()")
  eq(2, "def f:\n  if false: return 1\n  return 2\nf()")
  eq({x: 1, y:2}, "class p:\n  x int\n  y int\np(1 2)")
  eq(1, "enum ab:\n  a\n  b int\nmatch a:\n  a: 1\n  b v: v")
  eq(2, "enum ab:\n  a\n  b int\nmatch b(2):\n  a: 1\n  b v: v")
  eq(undefined, "dec add: int int int")
  eq(undefined, "interface addable a: add a a a")

  // Loop
  eq(3, "var i = 1\nwhile i < 3:\n  i += 1\ni")

  // Comment
  eq(1, "1 # line comment\n# whole line comment")

  console.warn("ok")
}

const test2 = () => {
  const fs = require("node:fs")
  const vm = require("node:vm")
  const child_process = require("node:child_process")
  const js = generate(parse(tokenize(fs.readFileSync(__dirname + "/moa.moa", "utf-8"))))
  const eq = (expected, moa) => {
    const go = new vm.Script(runtime + `;\n${js}\ncompile(${JSON.stringify(moa)})`).runInNewContext({console})
    fs.writeFileSync("/tmp/moa.go", go + "\n")
    const actual = child_process.execSync("go run /tmp/moa.go 2>&1").toString()
    if (actual !== expected) {
      throw new Error(`${actual} !== ${expected}`)
    }
    process.stderr.write(".")
  }
  eq("moa-go\n", 'log("moa-go")')
  console.warn("ok")
}

process.stderr.write("\x1B[2J\x1B[0f") // clear console
test1()
test2()
