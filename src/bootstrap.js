"use strict"
Error.stackTraceLimit = 30
const vm = require("vm")
const fs = require("fs")
const cp = require("child_process")

const trace = x => {
  console.dir(x, {depth: null})
  return x
}

const runtime = (() => {
  const io = {}
  const map = (...a) => new Map([...new Array(a.length / 2)].map((_, i) => [a[i*2], a[i*2+1]]))
  const fail = s => { throw new Error(s) }
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
          throw new Error(`Reading '${field}' but target is null`)
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
    io.glob = path => "main.moa parse.moa infer.moa genc.moa genjs.moa".split(" ")
    io.reads = path => fs.readFileSync(path).toString()
    io.puts = (...a) => { console.log(...a); return a[0] }
    io.log = (...a) => { console.error(a.map(x => JSON.stringify(x)).join(" ")); return a[0] }
    __tests.map(t => t())
    console.error(`PASS ${__tests.length} tests`)
    return main()
  }
}).toString().slice(8, -2) + ";\n"

const parse = (program, filename) => {
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
        yield {code, offset, indent, lineno, op1, op2, call, index, filename}
      }
      lineno += code.split(/\n/).length - 1
      offset += code.length
    }
  }
  const op2priorities = "+= || && == != < <= > >= + - * / %".split(" ")
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
        tokens[pos].index        ? link([tokens[pos++], t, ...untilby("]")]) :
        tokens[pos].call         ? (pos++, link([t].concat(untilby(")")))) :
        tokens[pos].op2          ? linkop2(tokens[pos++], t, bottom()) :
        t
      const priority = s => {
        const index = op2priorities.findIndex(op => op === s)
        if (index === -1) {
          throw new Error(`Unknown operator '${s}'`)
        }
        return index
      }
      const linkop2 = (op, l, r) =>
        Array.isArray(r) && r[0].op2 && priority(op.code) > priority(r[0].code) ? [r[0], [op, l, r[1]], r[2]] : [op, l, r]
      const t = tokens[pos++]
      return t.code === "(" ? link(untilby(")")[0]) :
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
    `"'`.includes(x.code[0]) ? genstring(x.code.slice(1, -1)) :
    x.code.startsWith("r/") ? x.code.slice(1) :
    x.code
  const genstring = s => JSON.stringify(s).replaceAll(/\\\\/g, "\\")
  const genmatch = a => a.length ?
    (a[0] === "_" ? "true" : "__target === " + a[0]) + " ? " + a[1] + " : \n  " + genmatch(a.slice(2)) :
    "(() => { throw new Error(`No match ${__target}`)})()"
  const geniif = (lineno, a) => a.length === 0 ? `(() => { throw new Error("No default in iif at ${lineno}")})()` :
    a.length === 1 ? a[0] :
    a[0] + " ? " + a[1] + " :\n  " + geniif(lineno, a.slice(2))
  const genstruct = a => "(" + a + ") => ({" + a + "})"
  const genobj = o => "JSON.stringify(" + o + ")"
  const genlhs = x => x?.[0]?.code === "."  ? genlhs(x[1]) + "." + genlhs(x[2]) : gen(x)
  const gencall = (head, tail) => {
    const code = head.code
    return code == "="                         ? genlhs(tail[0]) + " = " + gen(tail[1]) :
      code == "=="                             ? genobj(gen(tail[0])) + "===" + genobj(gen(tail[1])) :
      code == "!="                             ? genobj(gen(tail[0])) + "!==" + genobj(gen(tail[1])) :
      code === "."                             ? "__prop(" + gen(tail[0]) + ", " + JSON.stringify(gen(tail[1])) + ")" :
      code === "fn"                            ? "((" + tail.slice(0, -1).map(gen) + ") => " + gen(tail.at(-1)) + ")" :
      code === "var"                           ? "let "   + tail[0].code + " = " + gen(tail.at(-1)) :
      code === "let"                           ? "const " + tail[0].code + " = " + gen(tail.at(-1)) :
      code === "def"                           ? "const " + tail[0].code + " = (" + tail.slice(1, -1).map(gen) + ") => " + gen(tail.at(-1)) :
      code === "test"                          ? "__tests.push(() => " + gen(tail.at(-1)) + ")" :
      code === "struct"                        ? "const " + tail[0].code + " = " + genstruct(tail[1].slice(1).map(field => field[0].code)) :
      code === "each"                          ? "{let " + tail[0].code + " = -1; for (const " + tail[1].code + " of " + gen(tail[2]) + ") {" + tail[0].code + "++;" + gen(tail[3]) + "} }" :
      code === "while"                         ? "while (" + gen(tail[0]) + ") " + gen(tail[1]) :
      code === "if"                            ? "if (" + gen(tail[0]) + ")" + gen(tail[1]) + (tail[2] ? gen(tail.slice(2)) : "") :
      code === "else" && tail[0].code === "if" ? "else if (" + gen(tail[1]) + ") " + gen(tail[2]) :
      code === "else"                          ? "else " + gen(tail[0]) :
      code === "iif"                           ? "(" + geniif(head.lineno, tail.map(gen)) + ")" :
      code === "match"                         ? "(__target => " + genmatch(tail.slice(1).map(gen)) + ")(" + gen(tail[0]) + ")" :
      code === "assert"                        ? "(" + gen(tail[0]) + ") ? null : (() => { throw new Error(" + JSON.stringify("Assertion failed at " + head.filename + ":" + head.lineno) + ") })()" :
      code === "("                             ? "(" + gen(tail[0])   + ")" :
      code === "[" && head.index               ? gen(tail[0]) + "[" + tail.slice(1).map(gen)  + "]" :
      code === "["                             ? "[" + tail.map(gen)  + "]" :
      code === "{"                             ? "{\n" + generate(tail, level + 1) + "\n" + indent + "}" :
      head.op1                                 ? code + gen(tail[0]) :
      head.op2                                 ? "(" + gen(tail[0]) + code + gen(tail[1]) + ")" :
      gen(head) + "(" + tail.map(gen) + ")"
  }
  const genline = x => (s => (s.startsWith("else") ? "" : ";") + s)(gen(x))
  return indent + root.map(genline).join("\n" + indent)
}

const test = () => {
  const evaluate = js => {
    try {
      return new vm.Script(runtime + js).runInNewContext({})
    } catch(e) {
      return e.message
    }
  }
  const show = x => JSON.stringify(x, (_, v) => v?.constructor?.name === "Map" ? Object.fromEntries(v) : v)
  const eq = (src, expected) => {
    const node = parse(src, "test.moa")
    const js = generate(node)
    const actual = evaluate(js)
    if (show(expected) === show(actual)) {
      //console.log("| ok", src.replace(/\n/g, "\\n"))
    } else {
      console.log(`${show(expected)} is expected, but got ${show(actual)}`)
      console.log("src:", src)
      console.log("js:", js)
      console.dir(node, {depth: null})
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
  eq("map(1 true)", {1: true})
  eq("fn(1)()", 1)
  eq("fn(a a)(1)", 1)
  eq("fn(a a) 1", 1)

  // unray operator
  eq("!true", false)

  // binary operator
  eq("1 + 2", 3)
  eq("1 + 2 * 3", 7)
  eq("1 + (2 * 3)", 7)
  eq("(1 + 2) * 3", 9)
  eq("1 + 2 == 1 * 3", true)
  eq("[1] == [1]", true)
  eq("a = 1", 1)

  // embedded
  eq("assert true", null)
  eq("assert false", "Assertion failed at test.moa:1")

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
  eq("var a 0\neach _ b [1 2] { a += b }\na", 3)
  eq("var a 0\neach b _ [1 2] { a += b }\na", 1)
  eq("var a 0\nwhile a < 2 { a += 1 }\na", 2)

  // comment
  eq("1 # comment", 1)

  // edge case
  eq("let a [0]\na[0] = 1", 1)
}

const main = async () => {
  const node = "main.moa parse.moa infer.moa genc.moa genjs.moa".split(" ").flatMap(filename => parse(fs.readFileSync(__dirname + "/" + filename).toString(), filename))
  const js = generate(node)
  fs.writeFileSync("/tmp/moa", "#!node\n" + runtime + ";\n" + js + ";\n__main()")
  console.log(cp.execSync("node /tmp/moa version").toString().trim())
  //const c = cp.execSync("node /tmp/a.js c").toString()
  //fs.writeFileSync("/tmp/a.c", c)
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
