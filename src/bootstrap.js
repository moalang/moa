"use strict"
const assert = require("node:assert")
const fs = require("node:fs")
const ch = require("node:child_process")

const runtime = (() => {"use strict"
const __trace = []
const __log = (label, f) => {
  const ids = f.toString().match(/\([A-Za-z0-9_,\. ]*\)/)[0].slice(1, -1).split(",")
  return (...args) => {
    const a = structuredClone(args)
    const o = {
      args: args.map((a, i) => ids[i] + "=" + JSON.stringify(a)).join(", "),
      label: label,
    }
    __trace.push(o)
    return o.return = f(...args)
  }
}
const __fs = require("node:fs")
const __ch = require("node:child_process")
const match = (f, ...a) => __match(f(), a)
const __match = (v, a) => a.length === 0 ? (() => {throw new Error(`Not match '${v}'`)})() :
  a[0]() === v ? a[1]() :
  __match(v, a.slice(2))
const _ = true
const io = {
  argv: ["build"],
  puts: (...a) => console.log(...a),
  log: (...a) => console.error(...a),
  reads: (path) => __fs.readFileSync(path, "utf-8"),
  write: (path, content) => __fs.writeFileSync(path, content),
  glob: (..._) => ["moa.moa"],
  shell: cmd => __ch.execSync(cmd)
}
Array.prototype.fmap = function(f) { return this.flatMap(f) }
}).toString().slice(7, -1)

function tokenize(program) {
  const reg = /([0-9]+|(?:\.\.\.)?[A-Za-z0-9_]+|[\.()[\]{}]|"(?:[^"\\]|\\.)*"|`(?:[^`\\]|\\.)*`|[+\-*\/%<>!=^|&]+|#[^\n]*| +|\n+)/g
  let offset = 0
  let lineno = 1
  return program.match(reg).map(code => ({
    code,
    offset: offset += code.length,
    lineno: code.startsWith("\n") ? lineno += code.split("\n").length - 1 : lineno
  })).filter(t => t.code.trim())
}

function testTokenize() {
  const eq = (expected, program) => assert.strictEqual(tokenize(program).map(t => t.code).join(" "), expected)
  eq("0 a b1 ( ) [ ] { } \"A\" `B` + ** %= #comment", "0 a b1 ()[]{} \"A\" `B` + ** %= #comment")
  eq('"a\\"\nb"', '"a\\"\nb"')
  eq('`a\\`\nb`', '`a\\`\nb`')
}

function parse(tokens) {
  const then = (a, ...fs) => fs.length === 1 ? fs[0](...a) : then(fs[0](...a), ...fs.slice(1))
  const fail = s => { throw new Error(s) }
  const isOp = i => /^[+\-*\/%<>!=^|&]/.test(tokens[i]?.code)
  const isOp1 = i => isOp(i) && tokens[i].offset === tokens[i+1]?.offset - tokens[i+1]?.code?.length
  const isOp2 = i => isOp(i) && !isOp1(i)
  const fake = i => ({...tokens[i], code: "__" + tokens[i].code})
  const closeLeft = i => tokens[i-1].offset === tokens[i].offset - tokens[i].code.length
  const until = (i, end, a) => untilBy(i, end, a, unit)
  const untilBy = (i, end, a, f) =>
    i >= tokens.length ? fail(`Not closed '${end}'`) :
    tokens[i]?.code === end ? [i+1, a] :
    then(f(i), (j, node) => untilBy(j, end, a.concat([node]), f))
  const link = (i, node) =>
    tokens[i]?.code === "."                 ? link(i+2, [tokens[i], node, tokens[i+1]]) :
    tokens[i]?.code === "(" && closeLeft(i) ? then(until(i+1, ")", [node]), link) :
    tokens[i]?.code === "[" && closeLeft(i) ? then(until(i+1, "]", [fake(i), node]), link) :
    isOp2(i)                                ? then(unit(i+1), (j, next) => link(j, [tokens[i], node, next])) :
    [i, node]
  const unit = i =>
    isOp1(i)                ? then(unit(i+1), (j, node) => [j, [tokens[i], node]], link) :
    tokens[i]?.code === "(" && isOp(i+1) ? then(untilBy(i+1, ")", [tokens[i]], j => [j+1, tokens[j]]), link) :
    tokens[i]?.code === "(" ? then(until(i+1, ")", [tokens[i]]), link) :
    tokens[i]?.code === "[" ? then(until(i+1, "]", [tokens[i]]), link) :
    tokens[i]?.code === "{" ? walk(i+1, [tokens[i]]) :
    link(i+1, tokens[i])
  const line = (i, lineno, acc) =>
    tokens[i]?.lineno === lineno ? then(unit(i), (j, node) => line(j, lineno, acc.concat([node]))) :
    [i, acc.length === 1         ? acc[0] : acc]
  const walk = (i, acc) =>
    tokens[i]?.code === "}" ? [i+1, acc] :
    tokens[i]               ? then(line(i, tokens[i].lineno, []), (j, a) => walk(j, acc.concat([a]))) :
    [i, acc]
  return walk(0, [])[1]
}

function testParse() {
  const show = x => Array.isArray(x) ? "(" + (x[0]?.code === "(" ? show(x[1]) : x.map(show).join(" ")) + ")" : x.code
  const eq = (expected, program) => assert.strictEqual(show(parse(tokenize(program))[0]), expected)

  eq("a", "a")
  eq("(a)", "(a)")
  eq("(a b)", "a b")
  eq("(a b)", "a(b)")
  eq("([ a)", "[a]")
  eq("(__[ a b)", "a[b]")
  eq("(+ a)", "+a")
  eq("(+ a b)", "a + b")
  eq("(+ a (- b))", "a + -b")
  eq("(+ a (* b c))", "a + b * c")
  eq("(* ((+ a b)) c)", "(a + b) * c")
  eq("(. a b)", "a.b")
  eq("((. a b) c)", "a.b(c)")
  eq("((. ((. a b) c) d) e)", "a.b(c).d(e)")
  eq("(__[ (. a b) c)", "a.b[c]")
  eq("({ a (b c))", "{\na\nb c\n}")
  eq("(a (__[ b c))", "a b[c]")
  eq('(a `\n`)', 'a `\n`')
}

function genjs(nodes) {
  const isOp = code => /^[+\-*\/%<>!=^|&]/.test(code)
  const genop1 = op => op
  const genop2 = op => op === "++" ? "+" : op
  const gencode = code => code
  const gencall = (head, tail) =>
    isOp(head.code) ? (tail.length === 2 ? gen(tail[0]) + genop2(head.code) + gen(tail[1]) : genop1(head.code) + gen(tail[0])) :
    head.code === "let"   ? `const ${gen(tail[0])} = ${gen(tail[1])}` :
    head.code === "var"   ? `let ${gen(tail[0])} = ${gen(tail[1])}` :
    head.code === "def"   ? `const ${gen(tail[0])} = __log("${head.lineno}: ${gen(tail[0])}", (${tail.slice(1, -1).map(gen)}) => ${gen(tail.at(-1))})` :
    head.code === "if"    ? `if (${gen(tail[0])}) ${gen(tail[1])}` :
    head.code === "match" ? `match(${tail.map(gen).map(s => `() => ${s}\n`)})` :
    head.code === "fn"    ? `(${tail.slice(0, -1).map(gen)}) => ${gen(tail.at(-1))}` :
    head.code === "__["   ? `${gen(tail[0])}[${gen(tail[1])}]` :
    head.code === "("     ? `(${gen(tail[0])})` :
    head.code === "["     ? `[${tail.map(gen)}]` :
    head.code === "{"     ? `{\n${genjs(tail)}\n}` :
    head.code === "."     ? `${gen(tail[0])}.${gen(tail[1])}` :
    gen(head) + "(" + tail.map(gen) + ")"
  const gen = node => Array.isArray(node) ? gencall(node[0], node.slice(1)) : gencode(node.code)
  return nodes.map(gen).join("\n")
}

function testGenjs() {
  const eq = (expected, program) => assert.strictEqual(genjs(parse(tokenize(program))), expected)
  eq("a", "a")
  eq("!a", "!a")
  eq("a+b", "a + b")
  eq('[a]', "[a]")
  eq("a[b]", "a[b]")
  eq("a.b", "a.b")
  eq("a.b[c]", "a.b[c]")
  eq('const a = __log("1: a", () => {\n\n})', "def a {}")
  eq("match(() => 1\n,() => 2\n,() => 3\n)", "match(1 2 3)")
  eq("const a = 1", "let a 1")
  eq("let a = 1", "var a 1")
  eq("(a) => a", "fn(a a)")
}

function main() {
  const moa = fs.readFileSync(__dirname + "/moa.moa", "utf-8")
  const js = runtime + genjs(parse(tokenize(moa)))
  fs.writeFileSync("/tmp/moa_bootstrap.js", js + '\nmain()\nconsole.error(__trace.map(t => t.label + "(" + t.args + ") -> " + JSON.stringify(t.return)).join("\\n"))')
  process.stdout.write(ch.execSync("node /tmp/moa_bootstrap.js").toString())
}

function trace(x) {
  console.dir(x, {depth:null})
  return x
}

testTokenize()
testParse()
testGenjs()
main()
