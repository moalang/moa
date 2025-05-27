"use strict"
const vm = require("vm")
const fs = require("fs")
const ps = require("child_process")

const log = x => { console.dir(x, {depth: null}); return x }
const parse = program => {
  function* tokenize() {
    let pos = 0
    let indent = 0
    let lineno = 1
    for (const code of program.split(/([A-Za-z_][A-Za-z0-9_]*|[0-9]+|""".*?"""|"[^"]*?"|```.*?```|`[^`]*?`|[ \r\n\t]+|[()\[\]{};]|#[^\n]*)/)) {
      if (/^[ \r\n\t#;]/.test(code) || code === "") {
        if (code.includes("\n")) {
          indent = code.split(/\n/).at(-1).length
          lineno += code.split(/\n/).length - 1
        }
      } else {
        const op = /^[+\-*/%|&<>=!]+$/.test(code) && code !== "=>"
        const lc = pos > 0 ? program[pos - 1] : ''
        const rc = program[pos + code.length]
        const op1 = (op || code === "...") && /[A-Za-z0-9_\]]/.test(rc)
        const op2 = op && !op1
        const call = code === "(" && /[A-Za-z0-9_"\]\)]/.test(lc)
        const index = code === "[" && /[A-Za-z0-9_"\]\)]/.test(lc)
        yield {code, op1, op2, call, index, indent, lineno}
      }
      pos += code.length
    }
  }
  const compose = tokens => {
    let pos = 0
    const fake = code => ({code})
    const until = (f, g=bottom) => [...function*() { while (pos < tokens.length && f(tokens[pos])) { yield g() } }()]
    const untilby = by => (t => (pos++, t))(until(t => t.code !== by))
    const bottom = () => {
      const link = t =>
        pos >= tokens.length ? t :
        tokens[pos].code === "." ? link([tokens[pos++], t, tokens[pos++]]) :
        tokens[pos].index        ? (pos++, link([[fake("."), t, fake("at")], ...untilby("]")])) :
        tokens[pos].call         ? (pos++, link([t].concat(untilby(")")))) :
        tokens[pos].op2          ? link([tokens[pos++], t, bottom()]) :
        t
      const t = tokens[pos++]
      return t.code === "(" ? link([t, ...untilby(")")]) :
             t.code === "[" ? link([t, ...untilby("]")]) :
             t.code === "{" ? (x => (tokens[pos++], [t, ...x]))(top()) :
             t.op1          ? link([t, bottom()]) :
             link(t)
    }
    const line = () => (t => squash(until(u => t.lineno === u.lineno && u.code !== "}")))(tokens[pos])
    const top = () => until(t => t.code !== "}", line)
    const squash = (a, f=x=>x) => a.length === 1 ? a[0] : f(a)
    return top()
  }
  return compose([...tokenize()])
}
const generate = root => {
  const gen = x => Array.isArray(x) ? gencall(x[0], x.slice(1)) :
    `"'`.includes(x.code[0]) ? JSON.stringify(x.code.slice(1, -1)) :
    x.code
  const gencall = (head, tail) =>
    head.op1 ? head.code + gen(tail[0]) :
    head.op2 ? gen(tail[ 0]) + head.code + gen(tail[1]) :
    head.code === "."    ? "__prop(" + gen(tail[0]) + ", " + JSON.stringify(gen(tail[1])) + ")" :
    head.code === "fn"   ? "0 || ((" + tail.slice(0, -1).map(gen) + ") => " + gen(tail.at(-1)) + ")" :
    head.code === "var"  ? "let "   + tail[0].code + " = " + gen(tail.at(-1)) :
    head.code === "let"  ? "const " + tail[0].code + " = " + gen(tail.at(-1)) :
    head.code === "def"  ? "const " + tail[0].code + " = (" + tail.slice(1, -1).map(gen) + ") => " + gen(tail.at(-1)) :
    head.code === "if"   ? "if (" + gen(tail[0]) + ")" + gen(tail[1]) :
    head.code === "else" ? generate(tail) :
    head.code === "("    ? "(" + gen(tail[0])   + ")" :
    head.code === "["    ? "[" + tail.map(gen)  + "]" :
    head.code === "{"    ? "{" + generate(tail) + "}" :
    gen(head) + "(" + tail.map(gen) + ")"
  return root.map(gen).join(";\n")
}
const runtime = (() => {
const __prop = (obj, field) => {
  switch (`${obj?.constructor?.name} ${field}`) {
    case "String size": return obj.length
    case "String at"  : return n => obj[n]
    case "Array size" : return obj.length
    case "Array at"   : return n => obj[n]
    default           : return obj[field]
  }
}
const io = {}
}).toString().slice(8, -2) + ";\n"
const evaluate = js => {
  try {
    return new vm.Script(runtime + js).runInNewContext()
  } catch(e) {
    return e.message
  }
}
const test = () => {
  const show = x => JSON.stringify(x)
  const eq = (src, expected) => {
    const node = parse(src)
    const js = generate(node)
    const actual = evaluate(js)
    if (show(expected) === show(actual)) {
      console.log("ok", src.replace(/\n/g, "\\n"))
    } else {
      console.log(`${show(expected)} is expected, but got ${show(actual)}`)
      console.log(src)
      console.log(node)
      console.log(runtime)
      console.log(js)
      process.exit(1)
    }
  }
  eq("1", 1)
  eq("1 + 2", 3)
  eq('"a b"', "a b")
  eq("!true", false)
  eq("fn(1)()", 1)
  eq("fn(a a)(1)", 1)
  eq("fn(a a) 1", 1)
  eq('"a".size', 1)
  eq('"a".at(0)', "a")
  eq('[1][0]', 1)
  eq('"a"[0]', "a")
  eq("let a 1\na", 1)
  eq("var a 0\na += 1", 1)
  eq("var a 0\nif false { a += 1 }\na", 0)
  eq("var a 0\nif true { a += 1 }\na", 1)
  eq("def f 1\nf()", 1)
  eq("def f a a\nf(1)", 1)
  eq("def f a b a + b\nf(1\n2)", 3)
  eq("def f ...a a.at(0)\nf(1)", 1)
  eq("def f { return 1 }\nf()", 1)
  eq("def f {\nreturn 1\n}\nf()", 1)
  eq("1 + (2 * 3)", 7)
  eq("[]", [])
  eq("[1]", [1])
  eq("[1 2]", [1, 2])
}
const main = async () => {
  const moa = fs.readFileSync(__dirname + "/moa.moa").toString()
  const js = generate(parse(moa))
  fs.writeFileSync("/tmp/a.js", js + "\n;main()")
  try {
    ps.execSync("node /tmp/a.js")
  } catch (e) {
    process.exit(1)
  }
}
test()
main()
