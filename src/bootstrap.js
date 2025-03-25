"use strict"
// echo 'def main: io.puts "hello"' | node bootstrap.js | node | grep hello

const runtime = (() => {
const some = v => v
const none = undefined
const tuple = (...a) => a
const vec = (...a) => a
const set = (...a) => new Set(a)
const map = (...a) => new Map([...new Array(a.length / 2)].map((_, i) => [a[i*2], a[i*2+1]]))
const __prop = (obj, name, ...a) => {
  if (name === "then") {
    return obj === none ? none : a[0](obj)
  } else if (name === "or") {
    return obj === none ? a[0]() : obj
  } else if (name.match(/^[0-9]/)) {
    return obj[name]
  }
  return obj[name]
}
const __op2 = (op2, lhs, f) =>
  op2 === "|||" ? (lhs === none ? f() : lhs) :
  op2 === "==" ? __str(lhs) == __str(f()) :
  op2 === "!=" ? __str(lhs) != __str(f()) :
  op2 === "< " ? lhs <  f() :
  op2 === "<=" ? lhs <= f() :
  op2 === "> " ? lhs >  f() :
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
  const tokens = []
  for (const code of moa.split(/([ \n]+|[()\[\]{}]|[0-9+]+\.[0-9]+|[:.+\-*/%!=^|&?><]+|"[^"]*"|[ \n]+)/)) {
    if (code.trim()) {
      const op1 = code === "!"
      const op2 = "+-*/%|&<>=!".includes(code[0])
      const dot = code === "."
      tokens.push({code, lineno, offset, ...(op1 && {op1}), ...(op2 && {op2}), ...(dot && {dot})})
    }
    offset += code.length
    lineno += code.split("\n").length - 1
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
  const untilCode = code => until(t => t.code !== code, parseUnit, _ => ++pos)
  const parseUnit = () => {
    const t = tokens[pos++]
    if (!t) {
      throw new Error()
    }
    const call = t => {
      ++pos // drop "("
      const closely = tokens[pos-1].offset + tokens[pos-1].code.length === tokens[pos].offset
      const args = untilCode(")")
      return closely ? [t, ...args] : t
    }
    const link = t => pos >= tokens.length ? t :
      tokens[pos].op1 ? link([tokens[pos++], t]) :
      tokens[pos].op2 ? link([tokens[pos++], t, parseUnit()]) :
      tokens[pos].dot ? link([tokens[pos++], t, parseUnit()]) :
      tokens[pos].code === "(" ? link(call(t)) :
      t
    return link(t.code === "(" ? [{parenthesis: true}, untilCode(")")[0]] : t)
  }
  const parseLine = (t) => {
    const a = until(tt => t.lineno === tt.lineno && !")]}".includes(tt.code), parseUnit)
    if (a.length === 0) {
      throw new Error(`BUG a line has no elements ${pos} ${tokens.map(t => t.code)}`)
    }
    return a.length === 1 ? a[0] : a
  }
  return until(() => true, parseLine)
}

const generate = nodes => {
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
      } else if (head.dot) {
        const [field, ...args] = Array.isArray(tail[1]) ? tail[1].map(gen) : [gen(tail[1])]
        return `__prop(${gen(tail[0])}, "${field}"${args.map(s => " ," + s).join("")})`
      } else if (head.op2) {
        return head.code === "=>" ? `${gen(tail[0])} => ${gen(tail[1])}` :
          `__op2("${head.code}", ${gen(tail[0])}, () => ${gen(tail[1])})`
      } else if (head.parenthesis) {
        return "(" + gen(tail[0]) + ")"
      } else {
        return gen(head) + "(" + tail.map(gen).join(", ") + ")"
      }
    } else if (node === undefined) {
      return ""
    } else {
      return node.code
    }
  }
  return nodes.map(gen).join("\n")
}

if (process.argv[2] === "test") {
  const vm = require("node:vm")
  const assert = require('node:assert');
  const eq = (expected, moa) => {
    const tokens = tokenize(moa)
    const nodes = parse(tokens)
    const js = generate(nodes)
    let actual = null
    try {
      actual = new vm.Script(runtime + ";\n" + js).runInNewContext({console})
    } catch (e) {
      actual = e.stack
    }
    assert.deepEqual(actual, expected, moa + " -> " + js + "\n" + JSON.stringify({tokens, nodes}, null, 2))
    process.stdout.write(".")
  }
  // Primitive
  eq(1, "1")
  eq(1.1, "1.1")
  eq("hi", '"hi"')
  eq(true, "true")
  eq(1, "(a => a)(1)")

  // Container
  eq(1, "some(1)")
  eq(undefined, "none")
  eq("a", 'tuple(1 "a").1')
  eq([1, 2], "vec(1 2)")
  eq(new Set([1, 2]), "set(1 1 2)")
  eq(new Map([["a", 1]]), 'map("a" 1)')

  // Methods
  eq(1, "some(1) ||| none")
  eq(1, "none ||| some(1)")
  eq(undefined, "none ||| none")
  eq(undefined, "none.then(a => a + 1)")
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

  // Declare   : let var def class enum dec interface extern
  // Branch    : iif if else guard match
  // Flow      : return throw catch

  // Loop      : for each while continue break
  // Global    : log assert
  // Reserved  : __.* bytes regexp time duration stream num decimal array import export

  console.log("ok")
} else {
  const fs = require("node:fs")
  const moa = fs.readFileSync("/dev/stdin", "utf-8")
  console.log(generate(parse(tokenize(moa))))
}
/*
dec log t     : t ... t
dec assert a  : a a void
dec iif a     : ...[bool a] a
dec if a      : bool a void
dec else a    : a void
dec guard     : bool void
dec match a b : a ...[a b] b
dec for       : void
dec while     : bool void
dec continue  : void
dec break     : void
dec return a  : a a
dec throw a b : a b
dec catch a b : a fn[b a] a # b is union type of thrown types
*/
