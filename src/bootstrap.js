"use strict"
const assert = require("node:assert")

const log = x => { console.dir(x, {depth: null}); return x }

function tokenize(program) {
  const reg = /(r\/(?:[^\/\\]|\\.)*?\/[A-Za-z]*|[0-9]+(?:\.[0-9]+)|(?:\.\.\.)?[A-Za-z0-9_]+|[\.()[\]{}]|"(?:[^"\\]|\\.)*"|`(?:[^`\\]|\\.)*`|[+\-*\/%<>!=^|&]+|#[^\n]*| +|\n+)/g
  const tokens = []
  let offset = 0
  let lineno = 1
  for (const code of program.match(reg)) {
    if (code.match(/^[\n #]/)) {
      lineno += code.split("\n").length - 1
    } else {
      tokens.push({code, offset, lineno})
    }
    offset += code.length
  }
  return tokens
}

function testTokenize() {
  const eq = (expected, program) => assert.strictEqual(tokenize(program).map(t => t.code).join(" "), expected, program)
  eq("0 1.0 a b1 ( ) [ ] { } \"A\" `B` + ** %=", "0 1.0 a b1 ()[]{} \"A\" `B` + ** %= #comment")
  eq('"a\\"\\nb"', `"a\\\"\\nb"`)
  eq("r/a/", "r/a/")
  eq("r/\\\\/", "r/\\\\/")
  eq("r/a/g", "r/a/g")
  eq("r/a/ig", "r/a/ig")
}

function parse(tokens) {
  const then = (a, f) => f(...a)
  const fail = s => { throw new Error(s) }
  const isOp = i => /^[+\-*\/%<>!=^|&]/.test(tokens[i]?.code)
  const isOp1 = i => isOp(i) && touchesRight(i) && !touchesLeft(i)
  const isOp2 = i => isOp(i) && !isOp1(i)
  const fake = i => ({...tokens[i], code: "__" + tokens[i].code})
  const touchesLeft = i => i >= 1 && tokens[i-1].offset + tokens[i-1].code.length === tokens[i].offset
  const touchesRight = i => (i+1) < tokens.length && tokens[i].offset + tokens[i].code.length === tokens[i+1].offset
  const until = (i, end, a) =>
    i >= tokens.length ? fail(`Not closed '${end}'`) :
    tokens[i]?.code === end ? [i+1, a] :
    then(unit(i), (j, node) => until(j, end, a.concat([node])))
  const link = (i, node) =>
    tokens[i]?.code === "."                   ? link(i+2, [tokens[i], node, tokens[i+1]]) :
    tokens[i]?.code === "(" && touchesLeft(i) ? then(until(i+1, ")", [node]), link) :
    tokens[i]?.code === "[" && touchesLeft(i) ? then(until(i+1, "]", [fake(i), node]), link) :
    isOp2(i)                                  ? then(unit(i+1), (j, next) => link(j, [tokens[i], node, next])) :
    [i, node]
  const unit = i =>
    isOp1(i)                ? then(unit(i+1), (j, node) => [j, [tokens[i], node]]) :
    tokens[i]?.code === "(" ? then(until(i+1, ")", [tokens[i]]), link) :
    tokens[i]?.code === "[" ? then(until(i+1, "]", [tokens[i]]), link) :
    tokens[i]?.code === "{" ? then(lines(i+1, [tokens[i]]), (j, a) => tokens[j].code === "}" ? [j+1, a] : fail(`${tokens[i]} is not closed until ${tokens[j]}`)) :
    link(i+1, tokens[i])
  const line = (i, lineno, acc) =>
    tokens[i]?.lineno === lineno && tokens[i]?.code !== "}" ? then(unit(i), (j, node) => line(j, lineno, acc.concat([node]))) :
    [i, acc.length === 1                                    ? acc[0] : acc]
  const lines = (i, acc) =>
    tokens[i] ? then(line(i, tokens[i].lineno, []), (j, a) => a.length === 0 ? [j, acc] : lines(j, acc.concat([a]))) :
    [i, acc]
  return lines(0, [])[1]
}

function testParse() {
  const show = x => Array.isArray(x) ? "(" + (x[0]?.code === "(" ? show(x[1]) : x.map(show).join(" ")) + ")" : x.code
  const eq = (expected, program) => assert.strictEqual(parse(tokenize(program)).map(show).join("\n"), expected, program)
  eq("a", "a")
  eq("(a)", "(a)")
  eq("(a b)", "a b")
  eq("(a b)", "a(b)")
  eq("([ a)", "[a]")
  eq("(__[ a b)", "a[b]")
  eq("(= (__[ a b) c)", "a[b] = c")
  eq("(+ a)", "+a")
  eq("(+ a b)", "a+b")
  eq("(+ a b)", "a + b")
  eq("(+ a (- b))", "a + -b")
  eq("(+ a (* b c))", "a + b * c")
  eq("(* ((+ a b)) c)", "(a + b) * c")
  eq("(&& (a b) (! (c d)))", "a(b) && !c(d)")
  eq("(. a b)", "a.b")
  eq("((. a b) c)", "a.b(c)")
  eq("((. ((. a b) c) d) e)", "a.b(c).d(e)")
  eq("(__[ (. a b) c)", "a.b[c]")
  eq("({)", "{}")
  eq("(a ({))", "a {}")
  eq("(a ({ b))", "a {b}")
  eq("({ a (b c))", "{\na\nb c\n}")
  eq("(a (__[ b c))", "a b[c]")
  eq("(a `\n`)", 'a `\n`')
}

function genjs(nodes) {
  const isOp = code => /^[+\-*\/%<>!=^|&]/.test(code)
  const genop1 = op => op
  const genop2 = op => op === "++" ? "+" : op
  const gencode = code => code.startsWith("r/") ? code.slice(1) : code
  const genmatch = ([x, y, ...z]) => `__m === ${x} ? ${y} : ${z.length === 0 ? '__fail(`Unmatch ${__m}`)' : z.length === 1 ? z[0] : genmatch(z)}`
  const genclass = fields => `(${fields}) => ({${fields}})`
  const geniif = a => a.length === 1 ? a[0] : `${a[0]} ? ${a[1]} : ${geniif(a.slice(2))}`
  const gencall = (head, tail) =>
    isOp(head.code) ? (tail.length === 2 ? gen(tail[0]) + genop2(head.code) + gen(tail[1]) : genop1(head.code) + gen(tail[0])) :
    head.code === "test"  ? `__tests.push(${tail[0].code} => ${gen(tail.at(-1))})` :
    head.code === "class" ? `const ${gen(tail[0])} = ${genclass(tail[1].slice(1).map(f => f[0].code))}` :
    head.code === "let"   ? `const ${gen(tail[0])} = ${gen(tail[1])}` :
    head.code === "var"   ? `let ${gen(tail[0])} = ${gen(tail[1])}` :
    head.code === "def"   ? `const ${gen(tail[0])} = (${tail.slice(1, -1).map(gen)}) => ${gen(tail.at(-1))}` :
    head.code === "for"   ?  `for (let ${tail[0].code}=0; ${tail[0].code}<${gen(tail[1])}; ++${tail[0].code}) ${gen(tail[2])}` :
    head.code === "each"  ?  `for (const ${tail[0].code} of ${gen(tail[1])}) ${gen(tail[2])}` :
    head.code === "if"    ? `if (${gen(tail[0])}) ${gen(tail[1])}` :
    head.code === "else" && tail[0].code === "if"  ? `else if (${gen(tail[1])}) ${gen(tail[2])}` :
    head.code === "else"  ? "else " + gen(tail[0]) :
    head.code === "match" ? `(__m => ${genmatch(tail.slice(1).map(gen))})(${gen(tail[0])})` :
    head.code === "iif"   ? "(" + geniif(tail.map(gen)) + ")" :
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
  const eq = (expected, program) => assert.strictEqual(genjs(parse(tokenize(program))), expected, program)
  eq("a", "a")
  eq("/a/", "r/a/")
  eq('"a\\\\"', '"a\\\\"')
  eq("!a", "!a")
  eq("a+b", "a + b")
  eq('[a]', "[a]")
  eq("a[b]", "a[b]")
  eq("a[b]=c", "a[b] = c")
  eq("a.b", "a.b")
  eq("a.b[c]", "a.b[c]")
  eq("a()", "a()")
  eq("a.b()", "a.b()")
  eq("a(b)", "a(b)")
  eq("a.b(c)", "a.b(c)")
  eq("a(b)", "a b")
  eq("a.b(c)", "a.b c")
  eq('const a = () => {\n\n}', "def a {}")
  eq("(__m => __m === 2 ? 3 : __fail(`Unmatch ${__m}`))(1)", "match(1 2 3)")
  eq("(__m => __m === 2 ? 3 : 4)(1)", "match(1 2 3 4)")
  eq("(__m => __m === 2 ? 3 : __m === 4 ? 5 : 6)(1)", "match(1 2 3 4 5 6)")
  eq("const a = 1", "let a 1")
  eq("const a = /a/", "let a r/a/")
  eq("let a = 1", "var a 1")
  eq("(a) => a", "fn(a a)")
  eq("__tests.push(t => {\n\n})", "test t {}")
  eq("__tests.push(t => {\nt.log(1)\n})", "test t { t.log 1 }")
  eq("if (a) {\n\n}", "if a {}")
  eq("if (a) {\n\n}\nelse if (b) {\n\n}\nelse {\n\n}", "if a {}\nelse if b {}\nelse {}")
  eq("for (let i=0; i<1; ++i) {\n\n}", "for i 1 {}")
  eq("for (const x of xs) {\n\n}", "each x xs {}")
  eq("const a = (b) => ({b})", "class a { b c }")
  eq("const a = (b,e) => ({b,e})", "class a { b c\ne f }")
  eq("(a ? b : c)", "iif a b c")
}

const runtime = (() => {"use strict"
Error.stackTraceLimit = 20
const __assert = require("node:assert")
const io = {
  log: x => { console.error(JSON.stringify(x, null, 2)); return x }
}
const fail = s => { throw new Error(s) }
const tuple = (...a) => a
const map = (...a) => new Map([...new Array(a.length / 2)].map((_, i) => [a[i*2], a[i*2+1]]))
const assert = (a, b) => a || fail(`AssertionError: ${b}`)
Array.prototype.fmap = function(f) { return this.flatMap(f) }
Array.prototype.tmap = function(f) { return f(...this) }
Object.defineProperty(Array.prototype, 'size', { get() { return this.length } })
String.prototype.starts = function(s) { return this.startsWith(s) }
String.prototype.count = function(s) { return this.split(s).length - 1 }
Object.defineProperty(String.prototype, 'size', { get() { return this.length } })
const __main = () => {
  console.log("skip self boot")
  return
  const fs = require("node:fs")
  const ch = require("node:child_process")
  const moa = fs.readFileSync("./moa.moa", "utf-8")
  const nodes = tokenize(moa, "moa.moa")
  infer(nodes)
  const js = generate(nodes)
  fs.writeFileSync("/tmp/a", "#!node\n" + js)
  ch.execSync("chmod 0755 /tmp/a")
  ch.execSync("node -c /tmp/a")
}
let __test_count = 0
const __tester = {
  log: (...a) => console.error(...a),
  eq: (a, b, c) => {
    __assert.strictEqual(a, b, c) && ++__test_count
    ++__test_count
  }
}
const __tests = []
const __test = () => {
  const t = new Date()
  __tests.map(f => f(__tester))
  console.log(`PASS ${__test_count} ${new Date() - t}ms`)
}
}).toString().slice(7, -1)

function main() {
  const fs = require("node:fs")
  const ch = require("node:child_process")
  const moa = fs.readFileSync(__dirname + "/moa.moa", "utf-8")
  const js = genjs(parse(tokenize(moa)))
  fs.writeFileSync("/tmp/moa_bootstrap.js", runtime + js + '\n__test()\n__main()')
  try {
    ch.execSync("node /tmp/moa_bootstrap.js js")
  } catch (e) {
    process.exit(e.status)
  }
}

testTokenize()
testParse()
testGenjs()
main()
