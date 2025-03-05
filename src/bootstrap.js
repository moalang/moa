"use strict"
// echo 'def main: io.puts "hello"' | node bootstrap.js | node | grep hello

const runtime = (() => {
const some = v => v
const none = "__none__"
}).toString().slice("() => {".length, -1).trim()

const tokenize = moa => {
  let lineno = 1
  let offset = 0
  const tokens = []
  for (const code of moa.split(/([ \n]+|[()\[\]{}]|[0-9+]+\.[0-9]+|[:.+\-*/%!=^|&?]+|"[^"]*"|[ \n]+)/)) {
    if (code.trim()) {
      const op1 = code === "!"
      const op2 = "+-*/%|&<>=!".includes(code[0])
      tokens.push({code, lineno, offset, ...(op1 && {op1}), ...(op2 && {op2})})
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
  const parseUnit = () => {
    const t = tokens[pos++]
    if (!t) {
      throw new Error()
    }
    const link = t => {
      if (pos === tokens.length) {
        return t
      }
      const closely = t.offset + t.code === tokens[pos].offset
      return t.code === "(" ? link((a => closely ? [{call: true}, t, ...a] : a.length === 1 ? [{parenthesis: true}, a[0]] : a)(until(t => t.code !== ")", parseUnit, _ => ++pos))) :
             t.code === "[" ? link((a => closely ? [{at: true}  , t, ...a] : [t, ...a])(until(t => t.code !== "]", parseUnit, _ => ++pos))) :
             t.code === "{" ? link((a => closely ? [{part: true}, t, ...a] : [t, ...a])(until(t => t.code !== "}", parseLine, _ => ++pos))) :
             tokens[pos].op1 ? link([tokens[pos++], t]) :
             tokens[pos].op2 ? link([tokens[pos++], t, parseUnit()]) :
             t
    }
    return link(t)
  }
  const parseLine = (t) => {
    const a = until(tt => t.lineno === tt.lineno && !")]}".includes(tt.code), parseUnit)
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
      } else if (head.op2) {
        return gen(tail[0]) + head.code + gen(tail[1])
      } else if (head.parenthesis) {
        return "(" + gen(tail[0]) + ")"
      } else {
        return gen(head) + "(" + tail.map(gen) + ")"
      }
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
    const nodes = parse(tokenize(moa))
    const js = generate(nodes)
    let actual = null
    try {
      actual = new vm.Script(runtime + ";\n" + js).runInNewContext()
    } catch (e) {
      actual = e.stack
    }
    assert.deepEqual(actual, expected, moa + " -> " + js + "\n" + JSON.stringify(nodes, null, 2))
    process.stdout.write(".")
  }
  // Primitive
  eq(1, "1")
  eq(1.1, "1.1")
  eq("hi", '"hi"')
  eq(true, 'true')

  // Parenthesis
  eq(1, '(1)')
  eq([], '[]')
  eq([1, 2], '[1 2]')
  eq(1, '{1}')
  eq(2, '{1\n2}')

  // Expression
  eq(3, '1 + 2')
  eq(7, '1 + 2 * 3')
  eq(9, '(1 + 2) * 3')

  // Expression
  eq(3, '1 + 2')
  eq(7, '1 + 2 * 3')
  eq(9, '(1 + 2) * 3')

  // Container : option tuple list set dict
  eq("__none__", "none")
  // Declare   : let var def class enum dec interface extern
  // Branch    : iif if else guard match
  // Flow      : return throw catch
  eq(1, '{return 1\n2}')

  // Loop      : for each while continue break
  // Global    : log assert
  // Reserved  : __.* bytes regexp time duration stream num decimal array import export

  console.log("ok")
} else {
  const fs = require("node:fs")
  const moa = fs.readFileSync("/dev/stdin", "utf-8")
  console.log(generate(parse(tokenize(moa))))
}
