"use strict"

const fs = require("fs")
const vm = require("vm")
const child_process = require("child_process")

const log = x => {
  console.dir(x, {depth: null})
  return x
}

const showNode = node => Array.isArray(node) ? "(" + node.map(showNode).join(" ") + ")" : node.code
const showToken = t => t.code + "\t" + [t.op2 && "op2", t.op1 && "op1", t.call && "call", t.index && "index"].filter(s => s).join(",")

const runtime = (() => {
  "use strict"
  Object.defineProperty(String.prototype, "present", { get() { return this.length > 0 } })
  Object.defineProperty(String.prototype, "size", { get() { return this.length } })
  Object.defineProperty(String.prototype, "has", { get() { return s => this.includes(s) } })
  Object.defineProperty(RegExp.prototype, "split", { get() { return s => s.split(this) } })
  const log = (...a) => (a.map(x => console.dir(x, {depth: null})), a[0])
}).toString().slice("(() => {".length, -("})".length)) + ";\n"

const compile = program => {
  const tokenize = program => {
    const tokens = []
    let pos = 0
    let lineno = 1
    for (const code of program.split(/([A-Za-z_][A-Za-z0-9_]*|-?0x[A-Fa-f0-9]+|-?[0-9]+(?:(?:\.[0-9]+)|(?:e[0-9]+))?|""".*?"""|"[^"]*?"|```.*?```|`[^`]*?`|[ \r\n\t]+|[()\[\]{};]|#[^\n]*)/)) {
      if (/^[ \r\n\t#;]/.test(code) || code === "") {
        lineno += code.split(/\n|;/).length - 1
      } else {
        const op = /^[+\-*/%|&<>=!]+$/.test(code)
        const op2 = op && " \n\t".includes(program[pos + code.length])
        const op1 = op && !op2
        const dot = code === "."
        const call = code === "(" && (pos > 0 && !("\n\t ".includes(program[pos-1])))
        const index = code === "[" && (pos > 0 && !("\n\t ".includes(program[pos-1])))
        tokens.push({code, lineno, op, op1, op2, dot, call, index})
      }
      pos += code.length
    }
    return tokens
  }

  const parse = tokens => {
    let pos = 0
    const until = (f, g, h) => {
      const a = []
      while (pos < tokens.length && f(tokens[pos])) {
        a.push(g())
      }
      h && h()
      return a
    }
    const untilBy = code => until(t => t.code !== code, parseAtom, () => pos++)
    const drop = (node, code) => {
      if (pos < tokens.length && tokens[pos].code === code) {
        pos++
        return node
      } else {
        throw new Error(`No '${code}' after ${showNode(node)}`)
      }
    }
    const isClose = s => ")]}".includes(s[0])
    const parseAtom = () => {
      const link = node =>
        pos < tokens.length && tokens[pos].dot ? link([tokens[pos++], node, tokens[pos++]]) :
        pos < tokens.length && tokens[pos].op2 ? link([tokens[pos++], node, parseAtom()]) :
        pos < tokens.length && tokens[pos].call ? (pos++, link([node, ...untilBy(")")])) :
        pos < tokens.length && tokens[pos].index ? link([tokens[pos++], node, ...untilBy("]")]) :
        node
      const t = tokens[pos++]
      return t.code === "(" ? link([t, ...untilBy(")")]) :
        t.code === "[" ? link([t, ...untilBy("]")]) :
        t.code === "{" ? link([t, ...drop(parseTop(), "}")]) :
        t.op1 ? link([t, parseAtom()]) :
        link(t)
    }
    const parseLine = () => {
      const lineno = tokens[pos].lineno
      const a = until(t => t.lineno === lineno && !isClose(t.code), parseAtom)
      return a.length === 1 ? a[0] : a
    }
    const parseTop = () => until(t => !isClose(t.code), parseLine)
    return parseTop()
  }

  const generate = root => {
    const genif = a => a.length === 1 ? a[0] : `if (${a[0]}) { ${a[1]} }${a.length == 2 ? "" : " else " + genif(a.slice(2))}`
    const geniif = a => a.length === 1 ? a[0] : `${a[0]} ? ${a[1]} : ${geniif(a.slice(2))}`
    const genenum = a => a.map(gentag).join(";")
    const gentag = t => Array.isArray(t) ? `let ${t[0].code} = (__value) => ({__tag: "${t[0].code}", __value})` : `let ${t.code} = {__tag: "${t.code}"}`
    const genmatch = a => a.length === 0 ? "(() => { throw new Error(`No match for ${JSON.stringify(__target)}`) })()" :
      a.length === 1 ? gen(a[0]) :
      `__target.__tag === "${a[0].code}" ? ${gencase(a[1])} : ${genmatch(a.slice(2))}`
    const gencase = x => Array.isArray(x) && x[0].code === "fn" ? `${gen(x)}(__target.__value)` : gen(x)
    const genbody = x => Array.isArray(x) && "{ if while return".split(" ").includes(x[0].code) ? "{" + genreturn(x.slice(1).map(gen)) + "}" : gen(x)
    const genreturn = a => a.slice(0, -1).join(";") + ";" + (a.at(-1).startsWith("return") ? a.at(-1) : "return " + a.at(-1))
    const gen = node => {
      if (Array.isArray(node)) {
        const head = node[0]
        const tail = node.slice(1)
        return head.op2 ? gen(tail[0]) + head.code + gen(tail[1]) :
          head.dot ? gen(tail[0]) + "." + tail[1].code :
          head.index ? gen(tail[0]) + ".at(" + gen(tail[1]) + ")" :
          head.code === "("      ? "(" + gen(tail[0]) + ")" :
          head.code === "["      ? "[" + tail.map(gen) + "]" :
          head.code === "{"      ? tail.length === 1 ? gen(tail[0]) : tail.map(gen).join(";") :
          head.code === "fn"     ? `((${tail.slice(0, -1).map(gen)}) => ${genbody(tail.at(-1))})` :
          head.code === "iif"    ? geniif(tail.map(gen)) :
          head.code === "match"  ? `(__target => ${genmatch(tail.slice(1))})(${gen(tail[0])})` :
          head.code === "var"    ? `let ${tail[0][1].code} = ${gen(tail[0][2])}` :
          head.code === "let"    ? `const ${tail[0][1].code} = ${gen(tail[0][2])}` :
          head.code === "def"    ? `const ${tail[0].code} = (${tail.slice(1, -1).map(gen).join(", ")}) => ${genbody(tail.at(-1))}` :
          head.code === "enum"   ? `${genenum(tail[1].slice(1))}\nlet ${tail[0].code} = {${tail[1].slice(1).map(x => Array.isArray(x) ? x[0].code : x.code)}}` :
          head.code === "class"  ? `let ${tail[0].code} = ${(a => `(${a}) => ({${a}})`)(tail[1].slice(1).map(t => t[0].code))}` :
          head.code === "if"     ? genif(tail.map(gen)) :
          head.code === "each"   ? `for (const ${tail[0].code} of ${gen(tail[1])}) { ${gen(tail[2])} }` :
          head.code === "while"  ? `while (${gen(tail[0])}) { ${gen(tail[1])} }` :
          head.code === "throw"  ? `(() => { throw(${tail.map(gen)}) })()` :
          head.code === "catch"  ? `(() => { try { return ${gen(tail[0])} } catch (__e) { return (${gen(tail[1])})(__e) } })()` :
          gen(head) + "(" + tail.map(gen).join(", ") + ")"
      } else {
        const c = node.code
        return c.startsWith('"""') ? JSON.stringify(c.slice(3, -3)) :
          c.startsWith('```') ? new RegExp(c.slice(3, -3)) :
          c.startsWith("`") ? new RegExp(c.slice(1, -1)) :
          c.endsWith("?") ? c.slice(0, -1) :
          c
      }
    }
    return gen(root)
  }

  const tokens = tokenize(program)
  const trees = parse(tokens)
  const js = trees.map(generate).join(";")
  return { tokens, trees, js }
}

const runJs = (js, context={}) => {
  try {
    return new vm.Script(runtime + js).runInNewContext({console, ...context})
  } catch(e) {
    return `error: ${e}`
  }
}

const runTest = eq => {
  const test = (...a) => {
    const result = eq(...a)
    if (result === true) {
      process.stdout.write(".")
    } else {
      throw new Error(result)
    }
  }

  // Internal syntax
  test("true", true)
  test("1", 1)
  test("1.5", 1.5)
  test("-1", -1)
  test("-1.5", -1.5)
  test('"a"', "a")
  test('"a b"', "a b")

  // Primitives
  test("fn(1)()", 1)
  test("fn(a a)(1)", 1)
  test("fn(a b a + b)(1 2)", 3)
  test("fn(a b? a)(1)", 1)

  // Syntax sugars
  test("1 #a", 1)
  test("fn(a a) 1", 1)
  test("fn(a a) 1 #b", 1)
  test("1\nfn(a a) 2", 2)
  test("!true", false)
  test("1 + 2", 3)
  test("fn(1)()", 1)
  test('"abc".size', 3)
  test('"abc".slice(1)', "bc")
  test('"abc".slice(1 -1)',"b")
  test("1e2", 100)
  test("0xff", 255)
  test("-0xFF", -255)
  test('"""a"b"""', "a\"b")
  test("`[0-9]`", /[0-9]/)
  test("```1`2```", /1`2/)
  test("1 + 2 * 3", 7)
  test("1 * 2 + 3", 5)
  test("(1 + 2)", 3)
  test("(1 + 2) * 3", 9)
  test("1 + (2 * 3)", 7)
  test("{ 1 }", 1)
  test("{ fn(a a) 1 }", 1)
  test("{ 1; fn(a a) 2 }", 2)
  test("{ 1\nfn(a a) 2 }", 2)
  test("[1]", [1])
  test("[1 2]", [1, 2])
  test("[1][0]", 1)
  test("[1 2][-1]", 2)

  // Definitions
  test("let a = 1; a", 1)
  test("let f = fn(1); f()", 1)
  test("let f = fn(a a); f(2)", 2)
  test("def f 1; f()", 1)
  test("def f a a; f 1", 1)
  test("def f a b a + b; f 1 2", 3)
  test("def f a b { a + b }; f 1 2", 3)
  test("def f a b { return a + b }; f 1 2", 3)
  test("enum ab { a; b }; match a a 1", 1)
  test("enum ab { a; b int }; match b(2) a 1 b fn(n n)", 2)
  test("class ab { a int; b int }; ab(1 2).a", 1)
  test("class ab { a int; b int }; ab(1 2).b", 2)

  // Branches
  test("throw 1", "error: 1")
  test("catch throw(1) n => n", 1)
  test("iif true 1 2", 1)
  test("iif false 1 2", 2)
  test("iif false 1 true 2 3", 2)
  test("iif false 1 false 2 3", 3)
  test("iif false throw(1) 2", 2)
  test("iif false throw(1) true 2 throw(3)", 2)
  test("iif false throw(1) false throw(2) 3", 3)

  // Statements
  test("1; 2", 2)
  test("if true { throw 1 }", "error: 1")
  test("if false { throw 1 }; 2", 2)
  test("if false { throw 1 } true { throw 2 }", "error: 2")
  test("if false { throw 1 } false { throw 2 }; 3", 3)
  test("var n = 0; each m [1 2 3] n += m; n", 6)
  test("var n = 0; while n < 3 { n += 1; if n == 2 { break } }; n", 2)
  test("var n = 0; while n < 3 { n += 1; if true { continue }; throw(1) }; n", 3)
  test("var n = 0; while n < 3 { n += 1; if true { continue }; throw(1) }; n", 3)
  test("fn({1; 2})()", 2)
  test("fn({return 1; 2})()", 1)
  test("fn({1; return 2})()", 2)
  test("fn(if(true {return 1}))()", 1)
  test("fn(if(false 1 { return 2 }))()", 2)
  test("fn(while(true {return 1}))()", 1)

  // Debug
  test("log 1 2", 1)

  // Methods
  test('`,`.split("1,2")', ["1", "2"])
  test('"a".has("a")', true)
  test('"".present', false)
  test('"a".present', true)

  // Others
  test("let x = true && !false; x", true)
  console.log("ok")
}

const testInterpriter = () => runTest((code, expected) => {
  const {tokens, trees, js} = compile(code)
  const actual = runJs(js, {console: {dir: () => null}})
  const str = o => o.constructor.name === "RegExp" ? o.toString() : typeof o === "object" ? JSON.stringify(o) : o
  return (str(expected) === str(actual)) || `Test failed
Expected: ${expected}
  Actual: ${actual}
    Code: ${code}
      JS: ${js}
   Trees: ${trees.map(showNode)}
  Tokens: ${tokens.map(showToken).join("\n")}`
})


const testCompiler = () => {
  const js = compile(fs.readFileSync("moa.moa", "utf-8")).js + "; compile"
  const compileToGo = runJs(js)
  runTest((exp, expected) => {
    const goExp = compileToGo(exp)
    const goProgram = `package main; import "fmt"; func main() { fmt.Print(${goExp}) }`
    fs.writeFileSync("/tmp/moa.go", goProgram)
    const actual = child_process.execSync("go run /tmp/moa.go 2>&1", {encoding: "utf-8"})
    return expected.toString() === actual || `Test failed
Expected: ${expected}
  Actual: ${actual}
    Code: ${exp}
      Go: ${goExp}`
  })
}

testInterpriter()
testCompiler()
