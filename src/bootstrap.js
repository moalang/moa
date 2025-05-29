"use strict"
const vm = require("vm")
const fs = require("fs")
const cp = require("child_process")

const trace = x => {
  console.dir(x, {depth: null})
  return x
}

const parse = program => {
  function* tokenize() {
    let offset = 0
    let indent = 0
    let lineno = 1
    const reg = /(r\/(?:[^\/\\]|\\.)*\/|[A-Za-z_][A-Za-z0-9_]*|[0-9]+|"[^"]*?"|`[^`]*?`|[ \r\n\t]+|[()\[\]{};]|#[^\n]*)/
    for (const code of program.split(reg)) {
      if (/^[ \r\n\t#;]/.test(code) || code === "") {
        if (code.includes("\n")) {
          indent = code.split(/\n/).at(-1).length
        }
      } else {
        const op = /^[+\-*/%|&<>=!]+$/.test(code) && code !== "=>"
        const lc = offset > 0 ? program[offset - 1] : ''
        const rc = program[offset + code.length]
        const op1 = (op || code === "...") && /[A-Za-z0-9_"\[]/.test(rc)
        const op2 = op && !op1
        const call = code === "(" && /[A-Za-z0-9_"\]\)]/.test(lc)
        const index = code === "[" && /[A-Za-z0-9_"\]\)]/.test(lc)
        yield {code, offset, indent, lineno, op1, op2, call, index}
      }
      lineno += code.split(/\n/).length - 1
      offset += code.length
    }
  }
  const compose = tokens => {
    let pos = 0
    const fake = code => ({code})
    const until = (f, g=bottom) => [...function*() { while (pos < tokens.length && f(tokens[pos])) { yield g() } }()]
    const untilby = (code, g=bottom) => drop(code, pos, until(t => t.code !== code, g))
    const drop = (code, start, ret) => {
      if (code !== tokens[pos++]?.code) {
        throw new Error(`No close '${code}' from ${tokens[start].offset}`)
      }
      return ret
    }
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
             t.code === "{" ? link([t, ...untilby("}", line)]) :
             t.op1          ? link([t, bottom()]) :
             link(t)
    }
    const line = () => (t => squash(until(u => t.lineno === u.lineno && u.code !== "}")))(tokens[pos])
    const squash = (a, f=x=>x) => a.length === 1 ? a[0] : f(a)
    return until(t => t.code !== "}", line)
  }
  return compose([...tokenize()])
}

const generate = (root, level=0) => {
  const indent = "  ".repeat(level)
  const gen = x => Array.isArray(x) ? gencall(x[0], x.slice(1)) :
    `"'`.includes(x.code[0]) ? genstr(x.code.slice(1, -1)) :
    x.code === "_" ? "true" :
    x.code.startsWith("r/") ? x.code.slice(1) :
    x.code
  const genstr = s => JSON.stringify(s).replaceAll(/\\\\/g, "\\")
  const genmatch = a => a.length ?
    "__target === " + a[0] + " ? " + a[1] + ":" + genmatch(a.slice(2)) :
    "(() => { throw new Error(`No match ${__target}`)})()"
  const geniif = a => a.length === 0 ? "(() => { throw new Error(`No default in if`)})()" :
    a.length === 1 ? a[0] :
    a[0] + " ? " + a[1] + " :\n  " + geniif(a.slice(2))
  const genstruct = a => "(" + a + ") => ({" + a + "})"
  const gencall = (head, tail) =>
    head.op1                                        ? head.code + gen(tail[0]) :
    head.op2                                        ? gen(tail[0]) + head.code + gen(tail[1]) :
    head.code === "."                               ? "__prop(" + gen(tail[0]) + ", " + JSON.stringify(gen(tail[1])) + ")" :
    head.code === "fn"                              ? "0 || ((" + tail.slice(0, -1).map(gen) + ") => " + gen(tail.at(-1)) + ")" :
    head.code === "var"                             ? "let "   + tail[0].code + " = " + gen(tail.at(-1)) :
    head.code === "let"                             ? "const " + tail[0].code + " = " + gen(tail.at(-1)) :
    head.code === "def"                             ? "const " + tail[0].code + " = (" + tail.slice(1, -1).map(gen) + ") => " + gen(tail.at(-1)) :
    head.code === "test"                            ? "__tests.push(() => " + gen(tail[0]) + ")" :
    head.code === "struct"                          ? "const " + tail[0].code + " = " + genstruct(tail[1].slice(1).map(field => field[0].code)) :
    head.code === "each"                            ? "for (const " + tail[0].code + " of " + gen(tail[1]) + ") " + gen(tail[2]) :
    head.code === "while"                           ? "while (" + gen(tail[0]) + ") " + gen(tail[1]) :
    head.code === "if"                              ? "if (" + gen(tail[0]) + ")" + gen(tail[1]) + (tail[2] ? gen(tail.slice(2)) : "") :
    head.code === "else" && tail[0].code === "if"   ? "else if (" + gen(tail[1]) + ") " + gen(tail[2]) :
    head.code === "else"                            ? "else " + gen(tail[0]) :
    head.code === "iif"                             ? "0 || (" + geniif(tail.map(gen)) + ")" :
    head.code === "match"                           ? "0 || (__target => " + genmatch(tail.slice(1).map(gen)) + ")(" + gen(tail[0]) + ")" :
    head.code === "assert"                          ? "assert(" + gen(tail[0]) + ", " + head.lineno + ")" :
    head.code === "("                               ? "(" + gen(tail[0])   + ")" :
    head.code === "["                               ? "[" + tail.map(gen)  + "]" :
    head.code === "{"                               ? "{\n" + generate(tail, level + 1) + "\n" + indent + "}" :
    gen(head) + "(" + tail.map(gen) + ")"
  return indent + root.map(gen).join(";\n" + indent).replace(/};\n *else/g, "} else")
}

const runtime = (() => {
  const io = {}
  const assert = (cond, lineno) => cond ? null : (() => { throw new Error(`Assertion failed at ${lineno}`) })()
  const __tests = []
  const __prop = (obj, field) => {
    switch (`${obj?.constructor?.name} ${field}`) {
      case "String size"   : return obj.length
      case "String has"    : return s => obj.includes(s)
      case "String gsub"   : return (a, b) => obj.replaceAll(a, b)
      case "String string" : return obj
      case "String starts" : return s => obj.startsWith(s)
      case "String ends"   : return s => obj.ends(s)
      case "Array size"    : return obj.length
      case "Array fmap"    : return f => obj.flatMap(f)
      default:
        if (obj === undefined) {
          throw new Error(`BUG null.${field}`)
        }
        const value = obj[field]
        if (value === undefined) {
          throw new Error(`No '${field}' of ${JSON.stringify(obj)}`)
        }
        if (typeof value === "function") {
          return (...a) => obj[field](...a)
        }
        return value
    }
  }
  const __main = () => {
    const fs = require("fs")
    const cp = require("child_process")
    io.argv = process.argv.slice(2)
    io.glob = path => ["./moa.moa"]
    io.reads = path => fs.readFileSync(path).toString()
    io.puts = (...a) => { console.log(...a); return a[0] }
    io.log = (...a) => { console.error(a.map(x => JSON.stringify(x)).join(" ")); return a[0] }
    __tests.map(t => t())
    main()
  }
}).toString().slice(8, -2) + ";\n"

const test = () => {
  const evaluate = js => {
    try {
      return new vm.Script(runtime + js).runInNewContext({})
    } catch(e) {
      return e.message
    }
  }
  const show = x => JSON.stringify(x)
  const eq = (src, expected) => {
    const node = parse(src)
    const js = generate(node)
    const actual = evaluate(js)
    if (show(expected) === show(actual)) {
      console.log("| ok", src.replace(/\n/g, "\\n"))
    } else {
      console.log(`${show(expected)} is expected, but got ${show(actual)}`)
      console.log(src)
      console.dir(node, {depth: null})
      console.log(runtime)
      console.log(js)
      process.exit(1)
    }
  }

  // value
  eq("1", 1)
  eq('"a b"', "a b")
  eq('"\\n"', "\n")
  eq("true", true)
  eq("r/1/", /1/)
  eq("[]", [])
  eq("[1]", [1])
  eq("[1 2]", [1, 2])
  eq("fn(1)()", 1)
  eq("fn(a a)(1)", 1)
  eq("fn(a a) 1", 1)

  // unray operator
  eq("!true", false)

  // binary operator
  eq("1 + 2", 3)
  eq("1 + (2 * 3)", 7)

  // embedded
  eq("assert true", null)
  eq("assert false", "Assertion failed at 1")

  // method
  eq('[1][0]', 1)
  eq('"a"[0]', "a")
  eq('"a".size', 1)
  eq('"a".at(0)', "a")
  eq('"a a".gsub("a" "b")', "b b")
  eq('"a".at 0', "a")
  eq("r/1/.test(1)", true)

  // definition
  eq("let a 1\na", 1)
  eq("var a 0\na += 1", 1)
  eq("def f 1\nf()", 1)
  eq("def f a a\nf(1)", 1)
  eq("def f a b a + b\nf(1\n2)", 3)
  eq("def f ...a a.at(0)\nf(1)", 1)
  eq("def f { return 1 }\nf()", 1)
  eq("def f {\nreturn 1\n}\nf()", 1)
  eq("struct s { a int\n b bool }\ns 1 true", {a: 1, b: true})

  // branch
  eq("iif true 1 2", 1)
  eq("iif false 1 2", 2)
  eq("iif false 1 true 2 3", 2)
  eq("iif false 1 false 2 3", 3)
  eq("match 1 1 1 2 2", 1)

  // loop
  eq("var a 0\nif false { a += 1 }\na", 0)
  eq("var a 0\nif true { a += 1 }\na", 1)
  eq("var a 0\nif false { a += 1 } else { a += 2 }\na", 2)
  eq("var a 0\nif false { a += 1 } else if false { a += 2 }\na", 0)
  eq("var a 0\neach b [1 2] { a += b }\na", 3)
  eq("var a 0\nwhile a < 2 { a += 1 }\na", 2)

  // comment
  eq("1 # comment", 1)
}

const main = async () => {
  const moa = fs.readFileSync(__dirname + "/moa.moa").toString()
  const js = generate(parse(moa))
  fs.writeFileSync("/tmp/a.js", runtime + ";\n" + js + ";\n__main()")
  const c = cp.execSync("node /tmp/a.js c").toString()
  fs.writeFileSync("/tmp/a.c", c)
  //cp.execSync("cc /tmp/a.c -o /tmp/moa")
  //{
  //  const version = cp.execSync("/tmp/moa version").toString()
  //  if (version !== "moa0.0.1\n") {
  //    process.exit(1)
  //  }
  //  console.log("| ok c", version.trim())
  //}
  //{
  //  const moajs = cp.execSync("node /tmp/a.js js").toString()
  //  fs.writeFileSync("/tmp/moa.js", moajs)
  //  const version = cp.execSync("node /tmp/moa.js version").toString()
  //  if (version !== "moa0.0.1\n") {
  //    process.exit(1)
  //  }
  //  console.log("| ok js", version.trim())
  //}
}

test()
main()
