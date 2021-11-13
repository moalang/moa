'use strict'

const fs = require('fs')
const puts = (...a) => console.log(...a)
const dump = o => console.dir(o, {depth: null})
const trace = (...a) => (puts(...a), a[a.length - 1])
const str = JSON.stringify
const eq = (a,b) => str(a) === str(b)
const isArray = o => typeof o === 'object' && o.constructor === Array
const op2Assign = '= += -= *= /= ='.split(' ')
const isAssign = t => op2Assign.includes(t)
const op2 = '+ - * / % += -= *= /= %= == != <= >= < > && ||'.split(' ')
const isOp2 = t => op2.includes(t)
const isGlue = t => t === '=>' || isOp2(t) || isAssign(t)
const fail = (msg, o) => {dump(o); throw new Error(msg)}
const until = (f, g, a) => {
  a = a || []
  while (f()) {
    a.push(g())
  }
  return a
}

const tokenize = src => src.split(/([0-9]+|[a-zA-Z_][a-zA-Z_0-9]*(?:,[a-zA-Z_][a-zA-Z_0-9]*)*(?:\(\)?)?|[ \n]+|[.+\-*/=!&|<>]+|"[^"]*"|`[^`]*`|(?:#.*\n)|.)/).filter(t => t[0] !== '#').map(toToken).filter(t => t)
const toToken = t => t.match(/^ *\n/) ? t.slice(t.lastIndexOf('\n')) : t.trim().replace(/\\n/g, '\n').replace(/\\/g, '\\')
const parse = tokens => {
  let pos = 0
  const consume = f => pos < tokens.length ? f(tokens[pos++]) : fail('EOT', {f: f.toString(),tokens})
  const next = v => (++pos, v)
  const many = (f, g, a) => until(() => pos < tokens.length && g(tokens[pos]), f, a)
  const unit = () => consume(t =>
    t === '[' ? next(many(exp, t => t !== ']', ['array'])) :
    t === '(' ? next(many(exp, t => t !== ')')) :
    t.endsWith('(') ? [t.slice(0, -1)].concat(next(many(exp, t => t !== ')'))) :
    t === ':' ? (tokens[pos][0] === '\n' ? lines(tokens[pos++]) : [line()]) :
    t)
  const exp = () => suffix(unit())
  const suffix = u => {
    const t = tokens[pos++]
    return t === '.' ? suffix([t, u].concat(unit())) :
      t === '=>' && tokens[pos][0] === '\n' ? [t, u, ['do'].concat(lines(tokens[pos++]))] :
      isGlue(t) ? [t, u, exp()] :
      (--pos, u)
  }
  const line = () => many(exp, t => !'\n)'.includes(t[0]))
  const lines = indent => many(line, t => t === indent && ++pos, [line()])
  return lines('\n')
}
const generate = nodes => {
  const blocks = 'if for while return break continue'.split(' ')
  const addReturn = a => {
    if (!blocks.some(b => b === a[a.length - 1].slice(0, b.length))) {
      a[a.length - 1] = `return ${a[a.length - 1]}`
    }
    return a
  }
  const statement = a => a.length === 1 ? gen(a[0]) : `{\n  ${a.map(gen).join('  \n')}\n}`
  const exps = a => `{\n  ${addReturn(a.map(gen)).join('\n  ')}\n}`
  const newAdt = tag => `(__value => ({__tag: '${tag}', __value}))`
  const gen = node => {
    if (!isArray(node)) {
      if ('"`\''.includes(node[0])) {
        return str(node.slice(1, -1))
      } else {
        return node
      }
    }
    switch (node[0]) {
      case 'def': return `const ${node[1]} = (${node.slice(2, -1)}) => ${exps(node[node.length - 1])}`
      case 'test': return `__tests.push(${node[1]} => ${exps(node[2])})`
      case 'struct':
        const names = node[node.length - 1].map(field => field[0])
        return `const ${node[1]} = (${names}) => ({${names}})`
      case 'adt': return `const ${node[1]} = {${node[2].map(t => t[0] + ':' + newAdt(t[0]))}}`
      case 'array': return `[${node.slice(1).map(gen)}]`
      case 'var': return `let ${node[1]} = ${gen(node.slice(2))}`
      case 'let': return `const ${node[1]} = ${gen(node.slice(2))}`
      case 'iif': return `(${gen(node[1])} ? ${gen(node[2])} : ${gen(node[3])})`
      case 'if': return `if (${gen(node.slice(1, -1))}) ${statement(node[node.length - 1])}`
      case 'catch': return `__catch(() => ${gen(node[1])}, ${gen(node[2])})`
      case 'do': return `(() => ${exps(node.slice(1))})()`
      case 'match':
        const branch = a => `['${a[0]}', ${a[1]} => ${gen(a[2])}]`
        return `__match(${gen(node[1])}, [${node[2].map(branch)}])`
      case 'for':
        const a = node[1]
        const b = gen(node[2])
        const c = statement(node[3])
        return `for (let ${a}=0; ${a}<${b}; ++${a}) {${c}}`
      case 'while': return `while (${gen(node.slice(1, -1))}) {${statement(node[node.length - 1])}}`
      case 'assert':
        const cond = gen(node[1])
        const then = node[2] ? gen(node[2]) : 'null'
        return `!(${cond}) ? error("assert: " + ${str(cond)}) : ${then}`
      case '.': return `__dot(() => ${gen(node[1])}, '${node[2]}', [${node.slice(3).map(gen)}])`
      default:
        if (node[0] === '/') {
          return `(__d => __d == 0 ? error('Zero division error') : ${gen(node[1])} / __d)(${gen(node[2])})`
        } else if (node[0] === '=>') {
          return `((${node[1]}) => ${gen(node[2])})`
        } else if (node[0] === '-' && node.length === 2) {
          return '-' + gen(node[1])
        } else if (isAssign(node[0])) {
          return node[1] + node[0] + gen(node[2])
        } else if (isOp2(node[0])) {
          if (node[0] === '==') {
            return `__eq(${gen(node[1])}, ${gen(node[2])})`
          } else {
            return gen(node[1]) + node[0] + gen(node[2])
          }
        } else {
          return gen(node[0]) + (node.length === 1 ? '' : `(${node.slice(1).map(gen)})`)
        }
    }
  }
  return nodes.map(gen).join('\n')
}
const escapeSTDIN = s => '(() => {/*' + s + '*/}).toString().slice(9, -3)'
const stdlib = stdin => `let __stdout = ''
const error = msg => { const e = new Error(msg); e.catchable = true; throw e }
const io = {
  print: o => __stdout += __toString(o) + '\\n',
  dump: o => __stdout += JSON.stringify(o, null, ' ') + '\\n',
  stdin: ${escapeSTDIN(stdin)},
}
const __fail = msg => { throw new Error(msg) }
const __eq = (l, r) => JSON.stringify(l) === JSON.stringify(r)
const __tests = []
const __testMain = () => {
  const tester = {
    eq: (expect, actual) => {
      if (__eq(expect, actual)) {
        process.stdout.write('.')
      } else {
        console.log('expect:', expect)
        console.log('actual:', actual)
        console.log('stdout:', __stdout)
        throw new Error('test was failed')
      }
    },
    print: o => console.log(o.toString()),
    dump: o => console.log(JSON.stringify(o)),
  }
  __tests.forEach(t => t(tester))
}
const __toString = o => (t =>
  t === 'string' ? o :
  t === 'object' ? (
    o.constructor === Array ? '[' + o.map(__toString).join(' ') + ']' :
    JSON.stringify(o)
  ) : o.toString()
)(typeof o)
const __ref = o => typeof o === 'function' && o.toString().startsWith('()') ? o() : o
const __bind = (o, f) => typeof f === 'function' ? f.bind(o) : f
const __catch = (f, g) => {
  try {
    return f()
  } catch (e) {
    if (e.catchable) {
      return g(e)
    } else {
      throw e
    }
  }
}
const __match = (target, conds) => {
  for (const [tag, f] of conds) {
    if (target.__tag === tag) {
      return f(target.__value)
    }
  }
  console.error(target)
  __fail('Non-exhaustive pattern in match')
}

const __dot = (f, label, arg) => {
  const ref = () => {
    const o = f()
    const t = typeof o
    if (t === 'string') {
      switch (label) {
      case 'size': return o.length
      case 'at': return i => i < o.length ? o[i] : error('Out of index')
      case 'split': return s => o.split(s)
      case 'contains': return s => o.includes(s)
      case 'replace': return (a,b) => o.replaceAll(a, b)
      }
    } else if (t === 'object') {
      if (o.constructor === Array) {
        switch (label) {
        case 'at': return i => i < o.length ? o[i] : error('Out of index')
        case 'size': return o.length
        case 'append': return arg => Array.isArray(arg) ? o.concat([arg]) : o.concat(arg)
        case 'concat': return arg => o.concat(arg)
        case 'join': return arg => o.join(arg)
        case 'map':
        case 'filter':
        case 'push': return arg => o[label](arg)
        case 'contains': return arg => o.includes(arg)
        }
      } else if (label in o) {
        return __bind(o, o[label])
      } else {
        __fail(JSON.stringify(o) + ' does not include ' + label)
      }
    } else {
      console.error(o.toString())
      __fail(t + ' does not include ' + label)
    }
  }
  const g = ref()
  return arg.length ? g(...arg) : g
}
`
const run = (src, option) => {
  const stdin = option.stdin || ''
  const target = option.target || 'main'
  const tokens = tokenize(src.trim())
  const nodes = parse(tokens)
  const js = generate(nodes)
  const runtime = stdlib(stdin) + js + `\nreturn {ret: ${target}(), stdout: __stdout}`
  let result = {tokens, nodes, js, runtime}
  try {
    return Object.assign(result, Function(runtime)())
  } catch (e) {
    return Object.assign(result, {ret: e.message, stdout: '', error: e})
  }
}

const test = () => {
  const exp = (expect, exp, ...defs) => test(o => o.ret, '', expect, exp, ...defs)
  const stdin = (expect, stdin, exp, ...defs) => test(o => o.ret, stdin, expect, exp, ...defs)
  const stdout = (expect, exp, ...defs) => test(o => o.stdout, '', expect, exp, ...defs)
  const test = (f, stdin, expect, exp, ...defs) => {
    const src = defs.concat([`def main: ${exp}`]).join('\n')
    const result = run(src, {stdin})
    if (eq(expect, f(result))) {
      process.stdout.write('.')
    } else {
      puts('src:', src)
      puts('js:', result.js)
      puts('expect:', expect)
      puts('return:', result.ret)
      puts('tokens:', result.tokens)
      dump(result.nodes)
      if (result.error) {
        fs.writeFileSync('/tmp/faild.js', result.runtime)
        puts('node /tmp/faild.js')
      }
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
  exp([1, 2], 'array 1 2')
  exp([1, 2], '[1 2]')
  exp(1, '(n => n) 1')
  exp(3, '(a,b => a + b) 1 2')
  exp(6, '(a,b,c => a + b + c) 1 2 3')

  // function
  exp(1, 'one()', 'def one: 1')
  exp(3, 'add 1 2', 'def add a b: a + b')
  exp(6, 'calc 2 3', 'def calc a b:\n  def mul a b: a * b\n  mul a b')
  exp(3, '\n  var a 1\n  def inc: a += 1\n  def twice f:\n    f()\n    f()\n  twice inc\n  a')

  // method
  exp(2, '[1].map(n => n + 1).at(0)')

  // struct
  exp({x:1, y:2}, 'vector2 1 2', 'struct vector2:\n  x int\n  y int')
  exp(2, '(vector2 1 2).y', 'struct vector2:\n  x int\n  y int')

  // algebraic data type
  exp({__tag: 'a', __value: 1}, 'ab.a(1)', 'adt ab:\n  a int\n  b string')
  exp(1, 'match ab.a(1):\n  a v: v\n  b s: s.size', 'adt ab:\n  a int\n  b string')
  exp(2, 'match ab.b("hi"):\n  a v: v\n  b s: s.size', 'adt ab:\n  a int\n  b string')

  // exp
  exp(3, '1 + 2')
  exp(7, '1 + 2 * 3')
  exp(5, '1 * 2 + 3')
  exp(true, '([1 2].size == 1 + 1) && [3 4].size == 2')
  exp(1, '\n  var n 0\n  n = 1\n  n')
  exp(true, 's(1) == s(1)', 'struct s: value int')

  // constant
  exp(2, '\n  let a inc 1\n  a', 'def inc a: a + 1')

  // variable
  exp(3, '\n  var a 1\n  a += 2\n  a')
  exp(3, '\n  var a 1\n  def inc: a += 1\n  inc()\n  inc()\n  a')

  // branch
  exp(1, 'iif true 1 2')
  exp(2, 'iif false 1 2')
  exp(2, 'iif (true && (1 == 2)) 1 2')

  // lambda block
  exp(2, '\n  let f n =>\n    n += 1\n    n\n  f 1')

  // for block
  exp(3, '\n  var n 0\n  for i 3: n+=1\n  n')
  exp(2, '\n  var n 0\n  for i [1 2].size: n+=1\n  n')

  // while block
  exp(3, '\n  var n 0\n  while n < 3: n+=1\n  n')

  // if block
  exp(3, '\n  var n 0\n  if true:\n    n+=1\n    n+=2\n  n')

  // do block
  exp(1, 'do(1)')
  exp(2, 'do(1 2)')

  // control flow
  exp(2, '\n  1\n  2')
  exp(1, '\n  return 1\n  2')
  exp(1, '\n  while true:\n    return 1')
  exp(3, '\n  var n 0\n  for i 5:\n    if i >= 3: break\n    n += 1\n  n')
  exp(1, '\n  var n 0\n  for i 5:\n    if i <= 3: continue\n    n += 1\n  n')

  // error handling
  exp('Zero division error', '\n  1/0\n  1')
  exp('error', '\n  error "error"\n  1')
  exp(1, 'catch(1 _ => 2)')
  exp(2, 'catch(error("fail") e => 2)')
  exp(1, '\n  assert 1<2\n  1')
  exp('assert: 1>2', '\n  assert 1>2\n  1')
  exp('assert: false', 'assert false 1')
  exp(1, 'assert true 1')

  // stdio
  stdin('standard input', 'standard input', 'io.stdin')
  stdout('hello\nworld\n', '\n  io.print "hello"\n  io.print "world"')
  stdout('[1 2]\n', '\n  io.print [1 2]')

  // int
  exp(-1, '(-1)')
  exp(0, '-1 + 1')
  exp(0, 'add 1 (-1)', 'def add a b: a + b')

  // string
  exp(2, '"hi".size')
  exp('i', '"hi".at(1)')
  exp('Out of index', '"hi".at(3)')
  exp(['a', 'b'], '"a,b".split(",")')
  exp(true, '"hi".contains("h")')
  exp(false, '"hi".contains("z")')
  exp('heo', '"hello".replace("l" "")')

  // array
  exp(2, '[1 2].size')
  exp([1, 2], '[1].append 2')
  exp([1, 2], '[1].concat [2]')
  exp([2, 3], '[1 2].map n => n + 1')
  exp([1, 3], '[1 2 3].filter n => (n % 2) == 1')
  exp(true, '[1 2].contains 1')
  exp(false, '[1 2].contains 3')

  // comment
  exp(1, 'one()', '# comment', 'def one: 1')

  return true
}
const miniTest = () => {
  const src = fs.readFileSync(__dirname + '/mini.moa', 'utf8')
  const result = run(src, {stdin: src, target: '__testMain'})
  process.stdout.write(result.stdout)
  if (result.error) {
    puts(result.error)
    puts('--')
    puts('node /tmp/moa-mini-test-failed.js')
    puts('--')
    fs.writeFileSync('/tmp/moa-mini-test-failed.js', result.runtime)
  }
  return true
}
const compile = () => {
  const mini = fs.readFileSync(__dirname + '/mini.moa', 'utf8').trim()
  const src = fs.readFileSync('/dev/stdin', 'utf8').trim()
  const result = run(mini, {stdin: src})
  process.stdout.write(result.stdout)
  if (result.error) {
    process.exit(1)
  }
}

switch (process.argv[2]) {
  case 'test': test() && miniTest() && puts('ok'); break
  case 'compile': compile(); break
  default:
    puts('Usage:')
    puts('  node bootstrap.js test')
    puts('  node bootstrap.js mini-test')
    puts('  node bootstrap.js compile')
}
