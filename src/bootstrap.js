"use strict"

// # Syntax sugars to transform to internal syntax
// - [ ] line: a #b         -> a
// - [ ] line: a b          -> ("__call" a b)
// - [ ] line: a b #c       -> ("__call" a b)
// - [ ] top: a\nb c        -> ("{" a ("__call" b c))
// - [ ] exp: - 1           -> ("-" 1)
// - [ ] exp: 1 + 2         -> ("+" 1 2)
// - [ ] exp: f()           -> ("__call" f)
// - [ ] exp: f(1)          -> ("__call" f 1)
// - [ ] exp: s.size        -> ("." s "size")
// - [ ] exp: s.slice(1)    -> ("." s "slice" 1)
// - [ ] atom: 1e2          -> 100
// - [ ] atom: 0xff         -> 255
// - [ ] atom: """a"b"""    -> "a\"b"
// - [ ] atom: `[0-9]`      -> ("regex" "[0-9]")
// - [ ] atom: a?           -> a?
// - [ ] atom: (1 + 2)      -> ("(" ("+" 1 2))
// - [ ] atom: { a }        -> ("{" a)
// - [ ] atom: { a b }      -> ("{" ("__call" a b))
// - [ ] atom: { a; b c }   -> ("{" a ("__call" b c))
// - [ ] atom: { a\n b c }  -> ("{" a ("__call" b c))

// # Internal Syntax
// atom:
// |  "(" atom* ")"
// | "-"? [0-9]+ ("." [0-9]+)?
// | '"' [^"]* '"'
// | [^ ]+

const vm = require("vm")

const log = x => {
  console.dir(x, {depth: null})
  return x
}

const showNode = node => Array.isArray(node) ? "(" + node.map(showNode).join(" ") + ")" : node.code
const showToken = t => t.code + "\t" + [t.op2 && "op2", t.op1 && "op1", t.call && "call"].filter(s => s).join(",")

const runtime = (() => {
  Object.defineProperty(String.prototype, "size", { get() { return this.length } })

}).toString().slice("(() => {".length, -("})".length)) + ";\n"

const compile = program => {
  const tokenize = program => {
    const tokens = []
    let pos = 0
    let lineno = 1
    for (const code of program.split(/([A-Za-z_][A-Za-z0-9_]*|-?0x[A-Fa-f0-9]+|-?[0-9]+(?:(?:\.[0-9]+)|(?:e[0-9]+))?|""".*?"""|"[^"]*?"|`[^`]*?`|[ \r\n\t]+|[()\[\]{}]|#[^\n]*)/)) {
      if (/^[ \r\n\t#]/.test(code) || code === "") {
        lineno += code.split("\n").length - 1
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
        pos < tokens.length && tokens[pos].call ? (pos++, link([{code: "__call"}, node, ...untilBy(")")])) :
        node
      const t = tokens[pos++]
      return t.code === "(" ? link([t, ...untilBy(")")]) :
        t.code === "{" ? link([t, ...drop(parseTop(), "}")]) : link(t)
    }
    const parseLine = () => {
      const lineno = tokens[pos].lineno
      const a = until(t => t.lineno === lineno && !isClose(t.code), parseAtom)
      return a.length === 1 ? a[0] : [{code: "__call"}, ...a]
    }
    const parseTop = () => until(t => !isClose(t.code), parseLine)
    return parseTop()
  }

  const generate = root => {
    const genreturn = a => {
      return `0,(() => { ${a.slice(0, -1).map(gen).join(";")};return ${gen(a.at(-1))} })()`
    }
    const gen = node => {
      if (Array.isArray(node)) {
        const head = node[0]
        const tail = node.slice(1)
        return tail.length === 0 ? gen(head) :
          head.code === "__call" ? gen(tail[0]) + "(" + tail.slice(1).map(gen).join(", ") + ")" :
          head.code === "("      ? "(" + gen(tail[0]) + ")" :
          head.code === "{"      ? tail.length === 1 ? gen(tail[0]) : genreturn(tail) :
          head.op2 ? gen(tail[0]) + head.code + gen(tail[1]) :
          head.dot ? gen(tail[0]) + "." + tail[1].code :
          JSON.stringify(node)
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

const test = () => {
  const safe = (f, g) => {
    try {
      return f()
    } catch(e) {
      return g(e)
    }
  }
  const eq = (code, expected, context={}) => {
    const {tokens, trees, js} = compile(code)
    const actual = safe(() => new vm.Script(runtime + js).runInNewContext(context), e => `error: ${e.message}`)
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

  // Test internal syntax
  eq("1", 1)
  eq("-1", -1)
  eq('"a"', "a")
  eq('"a b"', "a b")

  // Test syntax sugars
  const a = 1
  const b = 2
  const c = 3
  const s = "abc"
  const f = (...a) => a.reduce((acc, x) => acc + x, 0)
  eq("a",            1, {a})
  eq("a #b",         1, {a})
  eq("f a",          1, {f, a})
  eq("f a #b",       1, {f, a})
  eq("a\nf b",       2, {f, a, b})
  eq("1",            1)
  eq("1.5",          1.5)
  eq("-1",           -1)
  eq("1 + 2",        3)
  eq("f()",          0, {f})
  eq("f(1)",         1, {f})
  eq("s.size",       3, {s})
  eq("s.slice(1)",   "bc", {s})
  eq("s.slice(1 -1)","b", {s})
  eq("1e2",          100)
  eq("0xff",         255)
  eq("-0xFF",        -255)
  eq('"""a"b"""',    "a\"b")
  eq("`[0-9]`",      /[0-9]/)
  eq("1 + 2 * 3",    7)
  eq("1 * 2 + 3",    5)
  eq("(1 + 2)",      3)
  eq("(1 + 2) * 3",  9)
  eq("1 + (2 * 3)",  7)
//  eq("a?",           ["?", a])
  eq("{ a }",        1, {a})
  eq("{ f a }",      1, {f, a})
//  eq("{ a; b c }",   ["{", a, ["__call", b, c]])
  eq("{ a\nf b }",   2, {f, a, b})
  console.log("ok")
}

test()
