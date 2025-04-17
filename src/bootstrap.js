"use strict"

const fs = require("fs")
const vm = require("vm")
const child_process = require("child_process")

const log = x => {
  console.dir(x, {depth: null})
  return x
}

const showNode = node => Array.isArray(node) ? "(" + node.map(showNode).join(" ") + ")" : node.code
const showToken = t => t.code + "\t" + [t.op2 && "op2", t.op1 && "op1", t.call && "call"].filter(s => s).join(",")

const runtime = (() => {
  "use strict"
  Object.defineProperty(String.prototype, "size", { get() { return this.length } })
  const __throw = (...args) => { throw new MoaError(...args) }
  class MoaError extends Error {
    constructor(value, ...notes) {
      super(value.toString())
      this.value = value
      this.notes = notes
    }
  }
}).toString().slice("(() => {".length, -("})".length)) + ";\n"

const compile = program => {
  const tokenize = program => {
    const tokens = []
    let pos = 0
    let lineno = 1
    for (const code of program.split(/([A-Za-z_][A-Za-z0-9_]*|-?0x[A-Fa-f0-9]+|-?[0-9]+(?:(?:\.[0-9]+)|(?:e[0-9]+))?|""".*?"""|"[^"]*?"|`[^`]*?`|[ \r\n\t]+|[()\[\]{};]|#[^\n]*)/)) {
      if (/^[ \r\n\t#;]/.test(code) || code === "") {
        lineno += code.split(/\n|;/).length - 1
      } else {
        const op = "+-*/%|&<>=".includes(code[0])
        const op2 = op && " \n\t".includes(program[pos + code.length])
        const op1 = op && !op2
        const dot = code === "."
        const call = code === "(" && (pos > 0 && !("\n\t ".includes(program[pos-1])))
        tokens.push({code, lineno, op2, dot, call})
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
        node
      const t = tokens[pos++]
      return t.code === "(" ? link([t, ...untilBy(")")]) :
        t.code === "{" ? link([t, ...drop(parseTop(), "}")]) : link(t)
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
    const genreturn = a => `0,(() => { ${a.slice(0, -1).join(";")};return ${a.at(-1)} })()`
    const geniif = a => a.length === 1 ? a[0] : `${a[0]} ? ${a[1]} : ${geniif(a.slice(2))}`
    const gen = node => {
      if (Array.isArray(node)) {
        const head = node[0]
        const tail = node.slice(1)
        return head.op2 ? gen(tail[0]) + head.code + gen(tail[1]) :
          head.dot ? gen(tail[0]) + "." + tail[1].code :
          head.code === "("      ? "(" + gen(tail[0]) + ")" :
          head.code === "{"      ? tail.length === 1 ? gen(tail[0]) : genreturn(tail.map(gen)) :
          head.code === "fn"     ? `((${tail.slice(0, -1).map(gen)}) => ${gen(tail.at(-1))})` :
          head.code === "iif"    ? geniif(tail.map(gen)) :
          head.code === "throw"  ? `(() => { throw(${tail.map(gen)}) })()` :
          head.code === "catch"  ? `(() => { try { return ${gen(tail[0])} } catch (__e) { return (${gen(tail[1])})(__e) } })()` :
          head.code === "let"    ? `let ${tail[0][1].code} = ${gen(tail[0][2])}` :
          gen(head) + "(" + tail.map(gen).join(", ") + ")"
      } else {
        const c = node.code
        return c.startsWith('"""') ? JSON.stringify(c.slice(3, -3)) :
          c.startsWith("`") ? new RegExp(c.slice(1, -1)) :
          c
      }
    }
    return gen(root)
  }

  const tokens = tokenize(program)
  const trees = parse(tokens)
  const js = trees.map(generate).join("\n")
  return { tokens, trees, js }
}

const runJs = (js, context={}) => {
  try {
    return new vm.Script(runtime + js).runInNewContext(context)
  } catch(e) {
    return `error: ${e}`
  }
}

const runTest = eq => {
  const test = (...a) => eq(...a) && process.stdout.write(".")

  // Test internal syntax
  test("true", true)
  test("1", 1)
  test("-1", -1)
  test('"a"', "a")
  test('"a b"', "a b")

  // Test syntax sugars
  const a = 1
  const b = 2
  const c = 3
  const s = "abc"
  const f = (...a) => a.reduce((acc, x) => acc + x, 0)
  test("a",            1, {a})
  test("a #b",         1, {a})
  test("f a",          1, {f, a})
  test("f a #b",       1, {f, a})
  test("a\nf b",       2, {f, a, b})
  test("1",            1)
  test("1.5",          1.5)
  test("-1",           -1)
  test("1 + 2",        3)
  test("f()",          0, {f})
  test("f(1)",         1, {f})
  test("s.size",       3, {s})
  test("s.slice(1)",   "bc", {s})
  test("s.slice(1 -1)","b", {s})
  test("1e2",          100)
  test("0xff",         255)
  test("-0xFF",        -255)
  test('"""a"b"""',    "a\"b")
  test("`[0-9]`",      /[0-9]/)
  test("1 + 2 * 3",    7)
  test("1 * 2 + 3",    5)
  test("(1 + 2)",      3)
  test("(1 + 2) * 3",  9)
  test("1 + (2 * 3)",  7)
//  test("a?",           ["?", a])
  test("{ a }",        1, {a})
  test("{ f a }",      1, {f, a})
  test("{ a; f b }",   2, {f, a, b})
  test("{ a\nf b }",   2, {f, a, b})

  // Test primitives
  test("fn(1)()", 1)
  test("fn(a a)(1)", 1)
  test("fn(a b a + b)(1 2)", 3)

  // Test branch
  test("throw 1", "error: 1")
  test("catch throw(1) n => n", 1)
  test("iif true 1 2", 1)
  test("iif false throw(1) 2", 2)
  test("iif false throw(1) true 2 throw(3)", 2)
  test("iif false throw(1) false throw(2) 3", 3)

  // Test naming
  test("let a = 1; a", 1)
  test("let f = fn(1); f()", 1)
  test("let f = fn(a a); f(2)", 2)

  console.log("ok")
}

const testInterpriter = () => {
  const eq = (code, expected, context={}) => {
    const {tokens, trees, js} = compile(code)
    const actual = runJs(js, context)
    if (expected instanceof RegExp ? expected.toString() === actual.toString() : expected === actual) {
      process.stdout.write(".")
    } else {
      throw new Error(`Test failed
Expected: ${expected}
Actual: ${actual}
Code: ${code}
JS: ${js}
Trees: ${trees.map(showNode)}
Tokens: ${tokens.map(showToken).join("\n")}`)
    }
  }
  runTest(eq)
}

const testCompiler = () => {
  const js = compile(fs.readFileSync("moa.moa", "utf-8")).js + "; compile"
  const compileToGo = runJs(js)
  const eq = (exp, expected) => {
    const goExp = compileToGo(exp)
    const goProgram = `package main

import "fmt"

func main() {
	fmt.Print(${goExp})
}
`
    fs.writeFileSync("/tmp/moa.go", goProgram)
    const actual = child_process.execSync("go run /tmp/moa.go 2>&1", {encoding: "utf-8"})
    if (expected.toString() === actual) {
      process.stdout.write(".")
    } else {
      throw new Error(`Test failed
Expected: ${expected}
Actual: ${actual}
Code: ${exp}
Go: ${goExp}`)
    }
  }
  runTest(eq)
}

testInterpriter()
testCompiler()
