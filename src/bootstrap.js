'use strict'

let fs = require('fs')
const stdjs = (function() {
function puts(...a) { console.log(...a); return a[a.length - 1] }
function p(...a) { puts('#', a.map(o => JSON.stringify(o, null, 2).replace(/\n +/g, ' ')).join(' ')); return a[a.length -1] }
function pp(...a) { puts('#', a.map(o => JSON.stringify(o, null, 2)).join(' ')); return a[a.length - 1] }
function fail(m, ...a) { a.length && p(a); throw new Error(m) }
Object.defineProperty(String.prototype, 'size', { get() { return this.length } });
String.prototype.at = function (n) { return n >= this.length || n < -this.length ? fail('Out of index') : n >= 0 ? this[n] : this[this.length + n] }
String.prototype.contains = function (s) { return this.includes(s) }
String.prototype.sub = function (a, b) { return this.replaceAll(a, b) }
String.prototype.rsub = function (a, b) { return this.replaceAll(new RegExp(a, 'g'), b) }
String.prototype.rsplit = function (r) { return this.split(new RegExp(r, 'g')) }
Object.defineProperty(Array.prototype, 'size', { get() { return this.length } });
Array.prototype.at = String.prototype.at
Array.prototype.append = function (a) { return this.concat(a) }
Array.prototype.contains = function (s) { return this.includes(s) }
}).toString().slice(12, -1).trim()
eval(stdjs.replace(/function (\w+)/g, (_, f) => `global.${f} = function ${f}`))

function compile(src) {
  const reg = /([():\[\].]|[\+\-\*\/%&|!=><]+|""".*"""|```.*```|"[^"]*?"|`[^`]*?`|[ \n]+|[a-zA-Z0-9_,]+(?:\(\)|\(?)|#.+)/
  const tokens = src.trim().replace(/^#.+\n/g, '').split(reg).map(t => t.replace(/^ +/, '').replace(/^#.*/g, '').replace(/^[ \n]+\n/, '\n')).filter(x => x)
  const is_op2 = x => '+-*/%&|!=<>'.includes(x[0])
  const parse = () => {
    let pos = 0
    const next = o => { ++pos; return o }
    const until = f => {
      const a = []
      while (pos < tokens.length && f(tokens[pos])) {
        a.push(consume())
      }
      return a
    }
    const reads = f => until(t => until(t => t[0] === '\n') && f(t))
    const right = () => until(t => t[0] !== '\n')
    const sepby = f => [right()].concat(pos < tokens.length && f(tokens[pos]) ? sepby(next(f)) : [])
    const consume = () => {
      const priorities = '|| && == != > >= < <= * / % + -'.split(' ')
      const priority = op => priorities.findIndex(x => x === op)
      const op2 = (op, l, r) => Array.isArray(r) && is_op2(r[0]) && priority(op) > priority(r[0]) ? [r[0], [op, l, r[1]], r[2]] : [op, l, r]
      const t = tokens[pos++]
      const node =
        t === '(' ? next(reads(t => t !== ')')) :
        t.endsWith('(') ? [t.slice(0, -1)].concat(next(reads(t => t !== ')'))) :
        t === '[' ? ['array'].concat(next(reads(t => t !== ']'))) :
        t === ':' && tokens[pos][0] === '\n' ? ['do'].concat(top(tokens[pos++])) :
        t === ':' ? ['do'].concat([right()]) :
        t
      const predict = node => {
        const tt = tokens[pos] || ''
        const dot = t => t.endsWith('(') ? [['.', node, t.slice(0, -1)]].concat(next(reads(t => t !== ')'))) : ['.', node, t]
        return is_op2(tt) ? predict(op2(tokens[pos++], node, consume())) :
          tt === '.' ? ++pos && predict(dot(tokens[pos++])) :
          node
      }
      return predict(node)
    }
    const top = br => sepby(t => t === br)
    return top('\n')
  }
  const gen = node => {
    const exps = a => a.length === 1 ? gen(a[0]) : `(() => {\n  ${a.map((e, i) => (i === a.length - 1 ? 'return ' : '') + gen(e)).join('\n  ')}\n})()`
    const matcher = ([tag, alias, ...exp]) => `v.__tag === '${tag}' ? (${alias} => ${gen(exp)})(v.__value)`
    const branch = a => a.length == 0 ? fail('invalid branch') : a.length === 1 ? a[0] : `${a[0]} ? ${a[1]} : ` + branch(a.slice(2))
    const handle = ([a, b]) => `(() => { try { return ${a} } catch (__e) { return (${b})(__e) } })()`
    const apply = ([head, ...tail]) =>
      tail.length === 0 ? gen(head) :
        head === 'def' ? `const ${tail[0]} = (${tail.slice(1, -1)}) => ${gen(tail[tail.length - 1])}` :
        head === 'struct' ? (keys => `const ${tail[0]} = (${keys}) => ({${keys}})`)(tail[1].slice(1).map(a => a[0])) :
        head === 'adt' ? (tags => `const ${tail[0]} = {${tags.map(tag => `${tag}: __value => ({__tag: '${tag}', __value})`)}}`)(tail[1].slice(1).map(a => a[0])) :
        head === 'match' ? `(v => ${tail[1].slice(1).map(matcher).join(' : ')} : error(\`Does not match \${v}\`))(${gen(tail[0])})` :
        head === 'array' ? `[${tail.map(gen).join(', ')}]` :
        head === 'do' ? exps(tail) :
        head === 'var' ? `let ${tail[0]} = ${gen(tail.slice(1))}` :
        head === 'let' ? `const ${tail[0]} = ${gen(tail.slice(1))}` :
        head === 'if' ? `(${branch(tail.map(gen))})` :
        head === 'catch' ? handle(tail.map(gen)) :
        head === '=>' ? `((${tail[0] + ') => ' + gen(tail[1])})` :
        head === '.' ? `${gen(tail[0])}.${gen(tail[1])}` :
        head === '/' ? `(d => d === 0 ? fail('Zero division error') : ${gen(tail[0])} / d)(${gen(tail[1])})` :
        head === '==' ? `JSON.stringify(${gen(tail[0])}) === JSON.stringify(${gen(tail[1])})` :
        head === '-' && tail.length === 1 ? '-' + gen(tail[0]) :
        is_op2(head) ? gen(tail[0]) + head + gen(tail[1]) :
        gen(head) + '(' + tail.map(gen).join(', ') + ')'
    const quote = (q, s) => q + s.replace(RegExp(q, 'g'), c => '\\' + c) + q
    const escape = s => s.replace(/\\/g, '\\\\')
    return Array.isArray(node) ? (node.length === 0 ? '' : apply(node)) :
      node.startsWith('"""') ? quote('"', escape(node.slice(3, -3))) :
      node.startsWith('```') ? quote('`', escape(node.slice(3, -3))) :
      escape(node)
  }
  return parse().map(gen).join('\n')
}

const test = () => {
  const run = js => {
    try {
      return eval(js + '\nmain()')
    } catch (e) {
      return e.message
    }
  }
  const exp = (expected, exp, ...defs) => {
    const src = defs.concat([`def main: ${exp}`]).join('\n')
    const js = compile(src)
    const actual = run(js)
    if (JSON.stringify(expected) === JSON.stringify(actual)) {
      process.stdout.write('.')
    } else {
      puts('src:', src)
      puts('js:', js)
      puts('expected:', expected)
      puts('actual:', actual)
      process.exit(1)
    }
  }

  // literals
  exp(1, '1')
  exp('hi', '"hi"')
  exp('hi', '`hi`')
  exp('a`b', '```a`b```')
  exp('1 + 1 = 2', '`${a} + ${a} = ${b}`', 'let a 1', 'let b 2')
  exp('1 + 1 = 2', '```${a} + ${a} = ${b}```', 'let a 1', 'let b 2')
  exp([1, 2], '[1 2]')
  exp(1, '(n => n) 1')
  exp(3, '(a,b => a + b) 1 2')

  // method chain
  exp([2, 3], '[1 2].map(n => n + 1)')
  exp([2, 3], '[1 2 3].map(n => n + 1).filter(n => n <= 3)')

  // int
  exp(-1, '(-1)')
  exp(0, '-1 + 1')
  exp(0, 'add 1 (-1)', 'def add a b: a + b')

  // string
  exp(2, '"hi".size')
  exp('i', '"hi".at 1')
  exp('h', '"hi".at((-2))')
  exp('Out of index', '"hi".at 3')
  exp(['a', 'b'], '"a,b".split ","')
  exp(true, '"hi".contains "h"')
  exp(false, '"hi".contains "z"')
  exp('heo', '"hello".sub "l" ""')

  // string with regular expression
  exp('h_o', '"hello".rsub `[el]+` "_"')
  exp(['1', '+', '2'], '"1 + 2".rsplit(`([0-9\+])`).filter(x => x != "" && x != " ")')

  // array
  exp(2, '[1 2].size')
  exp([1, 2], '[1].append 2')
  exp([2, 3], '[1 2].map n => n + 1')
  exp([1, 3], '[1 2 3].filter n => (n % 2) == 1')
  exp(true, '[1 2].contains 1')
  exp(false, '[1 2].contains 3')

  // function
  exp(1, 'one()', 'def one: 1')
  exp(3, 'add 1 2', 'def add a b: a + b')
  exp(6, 'calc 2 3', `def calc a b:
  def mul a b: a * b
  mul a b`)
  exp(3, `
  var a 1
  def inc: a += 1
  def twice f:
    f()
    f()
  twice inc
  a`)

  // struct
  exp({x:1, y:2}, 'vector2 1 2', 'struct vector2:\n  x int\n  y int')
  exp(2, '(vector2 1 2).y', 'struct vector2:\n  x int\n  y int')

  // algebraic data type
  exp({__tag: 'a', __value: 1}, 'ab.a 1', 'adt ab:\n  a int\n  b string')
  exp(1, 'match (ab.a 1):\n  a v: v\n  b s: s.size', 'adt ab:\n  a int\n  b string')
  exp(2, 'match (ab.b "hi"):\n  a v: v\n  b s: s.size', 'adt ab:\n  a int\n  b string')

  // exp
  exp(3, '1 + 2')
  exp(7, '1 + 2 * 3')
  exp(5, '1 * 2 + 3')
  exp(true, '([1 2].size == 1 + 1) && [3 4].size == 2')
  exp(1, '\n  var n 0\n  n = 1\n  n')
  exp(true, '(s 1) == (s 1)', 'struct s: value int')

  // variable
  exp(3, '\n  var a 1\n  a += 2\n  a')
  exp(3, '\n  var a 1\n  def inc: a += 1\n  inc()\n  inc()\n  a')

  // constant
  exp(2, '\n  let a inc 1\n  a', 'def inc a: a + 1')
  exp(2, '\n  let a:\n    var b 1\n    b += 1\n    b\n  a')

  // branch
  exp(1, 'if true 1 2')
  exp(2, 'if false 1 2')
  exp(2, 'if (true && (1 == 2)) 1 2')

  // error handling
  exp('Zero division error', '\n  1/0\n  1')
  exp('error', '\n  fail "error"\n  1')
  exp(1, 'catch(1 _ => 2)')
  exp(2, 'catch((fail "error") e => 2)')

  // do block
  exp(1, 'do 1')
  exp(5, 'do 1 (2 + 3)')

  // comments
  exp(1, '1', '# this is a comment')

  // bug fixes
  exp('"', '`"`')
  exp('\n', '`\n`')
  exp('\\n', '`\\n`')
  exp('a"b', '"""a"b"""')
  exp('a`b', '```a`b```')
  exp(1, 'f()', 'def f:\n  let a 1\n\n  a')
  exp('# comment', '"# comment"', )
  exp(1, '1', '# comment 1', '# comment 2')
  exp(1, 'if(true\n1\n2)')
  exp('ell', '"hello".slice 1 (-1)')
  exp(2, 'if(true 1 2) + 1')

  puts('ok')
  return true
}

function install() {
  let moa = fs.readFileSync('moa.moa', 'utf8')
  let js = `#!/usr/bin/env node
'use strict'

// Expand primitives
${stdjs}

// User code
${compile(moa)}

// Main logic
function main(){
  let a = process.argv[2]
  if (a === 'build') {
    let fs = require('fs')
    let src = fs.readFileSync(process.argv[3], 'utf8')
    console.log(compile(src))
  } else if (a === 'version') {
    console.log('moa 0.0.1 js')
  } else {
    console.log(\`Moa is a tool for managing Moa source code.

  Usage:

    moa <command> [arguments]

  The commands are:

         build       compile packages and dependencies
         version     print Moa version\`)
  }
}
main()
`
  fs.writeFileSync(__dirname + '/../bin/moa', js)
}

test() && install()
