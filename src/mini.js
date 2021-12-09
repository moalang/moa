'use strict'

const puts = (...a) => console.log(...a)
const write = (...a) => process.stdout.write(a.map(x => x.toString()).join(' '))
const trace = (...a) => (console.dir({'TRACE': a}, {depth: null}), a[a.length - 1])

function compile_js(src) {
  const tokens = src.split(/([():\[\]]|[\+\-\*\/%&|=><\.]+|"[^"]*?"|`[^`]*?`|[ \n]+|[^() \n]+")/).map(t => t.replace(/^ +/, '')).filter(x => x)
  const is_op2 = x => x.match(/[\+\-\*\/%&|=><\.]/)
  const parse = () => {
    let pos = 0
    const next = o => { ++pos; return o }
    const until = f => {
      const a = []
      while (pos < tokens.length && tokens[pos][0] !== '\n' && f(tokens[pos])) {
        a.push(consume())
      }
      return a
    }
    const reads = f => until(f || (() => true))
    const sepby = f => [reads()].concat(pos < tokens.length && f(tokens[pos]) ? sepby(next(f)) : [])
    const consume = () => {
      const priorities = '|| && == != > >= < <= * / % + - .'.split(' ')
      const priority = op => priorities.findIndex(x => x === op)
      const op2 = (op, l, r) => Array.isArray(r) && is_op2(r[0]) && priority(op) > priority(r[0]) ? [r[0], [op, l, r[1]], r[2]] : [op, l, r]
      const t = tokens[pos++]
      const node = t === '(' ? next(reads(t => t !== ')')) :
        t === '[' ? ['array'].concat(next(reads(t => t !== ']'))) :
        t === ':' && tokens[pos][0] === '\n' ? ['do'].concat(top(tokens[pos++])) :
        t === ':' ? ['do'].concat([reads()]) :
        t
      return (tokens[pos] || '').match(/^[\+\-\*\/%&|=><\.]/) ? op2(tokens[pos++], node, consume()) : node
    }
    const top = br => sepby(t => t === br)
    return top('\n')
  }
  const exps = a => a.length === 1 ? gen(a[0]) : `{\n  ${a.map((e, i) => (i === a.length - 1 ? 'return ' : '') + gen(e)).join('\n  ')}\n}`
  const gen_matcher = ([tag, alias, ...exp]) => `v.__tag === '${tag}' ? (${alias} => ${gen(exp)})(v.__value)`
  const apply = ([head, ...tail]) =>
    tail.length === 0 ? gen(head) :
    head === 'def' ? `const ${tail[0]} = (${tail.slice(1, -1)}) => ${gen(tail[tail.length - 1])}` :
    head === 'struct' ? (keys => `const ${tail[0]} = (${keys}) => ({${keys}})`)(tail[1].slice(1).map(a => a[0])) :
    head === 'adt' ? (tags => `const ${tail[0]} = {${tags.map(tag => `${tag}: __value => ({__tag: '${tag}', __value})`)}}`)(tail[1].slice(1).map(a => a[0])) :
    head === 'match' ? `(v => ${tail[1].slice(1).map(gen_matcher).join(' : ')} : fail(\`Does not match \${v}\`))(${gen(tail[0])})` :
    head === 'array' ? `[${tail.map(gen).join(', ')}]` :
    head === 'do' ? exps(tail) :
    head === 'var' ? `let ${tail[0]} = ${gen(tail.slice(1))}` :
    head === '=>' ? `((${tail[0] + ') => ' + gen(tail[1])})` :
    head === '.' ? `${gen(tail[0])}.${tail[1]}` :
    head === '==' ? `JSON.stringify(${gen(tail[0])}) === JSON.stringify(${gen(tail[1])})` :
    !Array.isArray(head) && is_op2(head) ? gen(tail[0]) + head + gen(tail[1]) :
    gen(head) + '(' + tail.map(gen).join(', ') + ')'
  const gen = node => Array.isArray(node) ? apply(node) : node
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
    const js = compile_js(src)
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

  // primitives
  exp(1, '1')
  exp('hi', '"hi"')
  exp('hi', '`hi`')
  exp('"', '`"`')
  exp('\n', '`\n`')
  exp('\n', '`\\n`')
  exp([1, 2], '[1 2]')
  exp(1, '(n => n) 1')
  exp(3, '(a,b => a + b) 1 2')

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
  exp({__tag: 'a', __value: 1}, 'ab.a(1)', 'adt ab:\n  a int\n  b string')
  exp(1, 'match (ab.a 1):\n  a v: v\n  b s: s.length', 'adt ab:\n  a int\n  b string')
  exp(2, 'match (ab.b "hi"):\n  a v: v\n  b s: s.length', 'adt ab:\n  a int\n  b string')

  // method
  exp([2, 3], '[1 2].map(n => n + 1)')

  // exp
  exp(3, '1 + 2')
  exp(7, '1 + 2 * 3')
  exp(5, '1 * 2 + 3')
  exp(true, '([1 2].length == 1 + 1) && [3 4].length == 2')
  exp(1, '\n  var n 0\n  n = 1\n  n')
  exp(true, '(s 1) == (s 1)', 'struct s: value int')

//  // constant
//  exp(2, '\n  let a inc 1\n  a', 'def inc a: a + 1')
//
//  // variable
//  exp(3, '\n  var a 1\n  a += 2\n  a')
//  exp(3, '\n  var a 1\n  def inc: a += 1\n  inc()\n  inc()\n  a')
//
//  // branch
//  exp(1, 'iif true 1 2')
//  exp(2, 'iif false 1 2')
//  exp(2, 'iif (true && (1 == 2)) 1 2')
//
//  // lambda block
//  exp(2, '\n  let f n =>\n    n += 1\n    n\n  f 1')
//
//  // for block
//  exp(3, '\n  var n 0\n  for i 3: n+=1\n  n')
//  exp(2, '\n  var n 0\n  for i [1 2].size: n+=1\n  n')
//
//  // while block
//  exp(3, '\n  var n 0\n  while n < 3: n+=1\n  n')
//
//  // if block
//  exp(3, '\n  var n 0\n  if true:\n    n+=1\n    n+=2\n  n')
//
//  // do block
//  exp(1, 'do(1)')
//  exp(2, 'do(1 2)')
//
//  // control flow
//  exp(2, '\n  1\n  2')
//  exp(1, '\n  return 1\n  2')
//  exp(1, '\n  while true:\n    return 1')
//  exp(3, '\n  var n 0\n  for i 5:\n    if i >= 3: break\n    n += 1\n  n')
//  exp(1, '\n  var n 0\n  for i 5:\n    if i <= 3: continue\n    n += 1\n  n')
//
//  // error handling
//  exp('Zero division error', '\n  1/0\n  1')
//  exp('error', '\n  error "error"\n  1')
//  exp(1, 'catch(1 _ => 2)')
//  exp(2, 'catch(error("fail") e => 2)')
//  exp(1, '\n  assert 1<2\n  1')
//  exp('assert: 1>2', '\n  assert 1>2\n  1')
//  exp('assert: false', 'assert false 1')
//  exp(1, 'assert true 1')
//
//  // stdio
//  stdin('standard input', 'standard input', 'io.stdin')
//  stdout('hello\nworld\n', '\n  io.print "hello"\n  io.print "world"')
//  stdout('[1 2]\n', '\n  io.print [1 2]')
//
//  // int
//  exp(-1, '(-1)')
//  exp(0, '-1 + 1')
//  exp(0, 'add 1 (-1)', 'def add a b: a + b')
//
//  // string
//  exp(2, '"hi".size')
//  exp('i', '"hi".at(1)')
//  exp('Out of index', '"hi".at(3)')
//  exp(['a', 'b'], '"a,b".split(",")')
//  exp(true, '"hi".contains("h")')
//  exp(false, '"hi".contains("z")')
//  exp('heo', '"hello".replace("l" "")')
//
//  // array
//  exp(2, '[1 2].size')
//  exp([1, 2], '[1].append 2')
//  exp([1, 2], '[1].concat [2]')
//  exp([2, 3], '[1 2].map n => n + 1')
//  exp([1, 3], '[1 2 3].filter n => (n % 2) == 1')
//  exp(true, '[1 2].contains 1')
//  exp(false, '[1 2].contains 3')
//
//  // comment
//  exp(1, 'one()', '# comment', 'def one: 1')
  puts('ok')
}

test()
