"use strict"
const assert = require("node:assert")

const log = x => { console.dir(x, {depth: null}); return x }

function tokenize(program) {
  const reg = /(r\/(?:[^\/\\]|\\.)*?\/[A-Za-z]*|[0-9]+|(?:\.\.\.)?[A-Za-z0-9_]+|[\.()[\]{}]|"(?:[^"\\]|\\.)*"|`(?:[^`\\]|\\.)*`|[+\-*\/%<>!=^|&]+|#[^\n]*| +|\n+)/g
  let offset = 0
  let lineno = 1
  return program.match(reg).map(code => ({
    code,
    offset: offset += code.length,
    lineno: code.startsWith("\n") ? lineno += code.split("\n").length - 1 : lineno
  })).filter(t => t.code.trim().replace(/^#.*/, ""))
}

function testTokenize() {
  const eq = (expected, program) => assert.strictEqual(tokenize(program).map(t => t.code).join(" "), expected)
  eq("0 a b1 ( ) [ ] { } \"A\" `B` + ** %=", "0 a b1 ()[]{} \"A\" `B` + ** %= #comment")
  eq('"a\\"\\nb"', `"a\\\"\\nb"`)
  eq("r/a/", "r/a/")
  eq("r/\\\\/", "r/\\\\/")
  eq("r/a/g", "r/a/g")
  eq("r/a/ig", "r/a/ig")
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
    tokens[i]?.code === "{" ? then(lines(i+1, [tokens[i]]), (j, a) => tokens[j].code === "}" ? [j+1, a] : fail(`${tokens[i]} is not closed until ${tokens[j]}`)) :
    link(i+1, tokens[i])
  const line = (i, lineno, acc) =>
    tokens[i]?.lineno === lineno && tokens[i]?.code !== "}" ? then(unit(i), (j, node) => line(j, lineno, acc.concat([node]))) :
    [i, acc.length === 1                                    ? acc[0] : acc]
  const lines = (i, acc) =>
    tokens[i] ? then(line(i, tokens[i].lineno, []), (j, a) => a.length === 0 ? [j, acc] : lines(j, acc.concat([a]))) :
    [i, acc]
  const result = lines(0, [])
  return result[1]
}

function testParse() {
  const show = x => Array.isArray(x) ? "(" + (x[0]?.code === "(" ? show(x[1]) : x.map(show).join(" ")) + ")" : x.code
  const eq = (expected, program) => assert.strictEqual(parse(tokenize(program)).map(show).join("\n"), expected)
  eq("a", "a")
  eq("(a)", "(a)")
  eq("(a b)", "a b")
  eq("(a b)", "a(b)")
  eq("([ a)", "[a]")
  eq("(__[ a b)", "a[b]")
  eq("(= (__[ a b) c)", "a[b] = c")
  eq("(+ a)", "+a")
  eq("(+ a b)", "a + b")
  eq("(+ a (- b))", "a + -b")
  eq("(+ a (* b c))", "a + b * c")
  eq("(* ((+ a b)) c)", "(a + b) * c")
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
}

const runtime = (() => {"use strict"
const io = {
  log: (...a) => console.error(...a),
}
Array.prototype.fmap = function(f) { return this.flatMap(f) }
String.prototype.starts = function(s) { return this.startsWith(s) }
String.prototype.count = function(s) { return this.split(s).length }
Object.defineProperty(String.prototype, 'size', { get() { return this.length } })
const __assert = require("node:assert")
const __fail = (...a) => { throw new Error(`${JSON.stringify(a)}`) }
const __main = () => {
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
const __tester = {
  log: (...a) => console.error(...a),
  eq: (a, b, c) => __assert.strictEqual(a, b, c)
}
const __tests = []
const __test = () => __tests.map(f => f(__tester))
}).toString().slice(7, -1)

function main() {
  const fs = require("node:fs")
  const ch = require("node:child_process")
  const moa = fs.readFileSync(__dirname + "/moa.moa", "utf-8")
  const js = genjs(parse(tokenize(moa)))
  fs.writeFileSync("/tmp/moa_bootstrap.js", runtime + js + '\n__test()\n__main()')
  process.stdout.write(ch.execSync("node /tmp/moa_bootstrap.js js").toString())
}

testTokenize()
testParse()
testGenjs()
main()
