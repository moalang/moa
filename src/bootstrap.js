'use strict'

// TODO
// - error handling
// - stdio
// - API for int, string, array, dictionary and error

const puts = (...a) => console.log(...a)
const dump = o => console.dir(o, {depth: null})
const trace = (...a) => (puts(...a), a[a.length - 1])
const str = JSON.stringify
const eq = (a,b) => str(a) === str(b)
const isArray = o => typeof o === 'object' && o.constructor === Array
const op2 = '. + - * / += -= *= /= == != < > <= >= && ||'.split(' ')
const isOp2 = t => op2.includes(t)
const fail = (msg, o) => {dump(o); throw new Error(msg)}

const tokenize = src => src.split(/([0-9]+|[a-zA-Z_][a-zA-Z_0-9]*|[ \n]+|[.+\-*/=!&|]+|"[^"]*"|`[^"]*`|.)/).map(toToken).filter(t => t)
const toToken = t => t.includes('\n') ? t.slice(t.lastIndexOf('\n')) : t.trim()
const parse = tokens => {
  let pos = 0
  const consume = f => pos < tokens.length ? f(tokens[pos++]) : fail('EOT', {f: f.toString(),tokens})
  const next = v => (++pos, v)
  const many = (f, g, a) => {
    a = a || []
    while (pos < tokens.length && g(tokens[pos])) {
      const t = f(tokens[pos])
      if (isOp2(t) && a.length && pos < tokens.length && g(tokens[pos])) {
        a[a.length - 1] = [t, a[a.length - 1], f(tokens[pos])]
      } else {
        a.push(t)
      }
    }
    return a
  }
  const block = () => tokens[pos][0] === '\n' ? lines(tokens[pos++]) : [line()]
  const unit = () => consume(t =>
    t === '(' ? next(many(unit, t => t !== ')')) :
    t === ':' ? block() :
    t)
  const lines = indent => many(line, t => t === indent && ++pos, [line()])
  const line = () => many(unit, t => t[0] !== '\n')
  return lines('\n')
}
const generate = nodes => {
  const dict = a => '{' + [...Array(a.length / 2).keys()].map(i => i * 2).map(i => `[${a[i]}]:${a[i+1]}`) + '}'
  const addReturn = a => (a[a.length-1] = `return ${a[a.length-1]}`,a)
  const statement = a => `{${a.map(gen).join(';')}}`
  const exps = a => `{${addReturn(a.map(gen)).join(';')}}`
  const gen = node => {
    if (!isArray(node)) {
      return node
    }
    switch (node[0]) {
      case 'def': return `const ${node[1]} = (${node.slice(2, -1)}) => ${exps(node[node.length - 1])}`
      case 'struct':
        const names = node[node.length - 1].map(field => field[0])
        return `const ${node[1]} = (${names}) => ({${names}})`
      case 'array': return `[${node.slice(1).map(gen)}]`
      case 'dict': return `(${dict(node.slice(1).map(gen))})`
      case 'var': return `let ${node[1]} = ${gen(node.slice(2))}`
      case 'let': return `const ${node[1]} = ${gen(node.slice(2))}`
      case 'if':
        if (node.length === 4) {
          return `(${gen(node[1])} ? ${gen(node[2])} : ${gen(node[3])})`
        } else if (node.length === 3) {
          return `if (${gen(node[1])}) ${statement(node[2])}`
        } else {
          fail('Unknown format of if', {node})
        }
      case 'for':
        if (node.length === 4) {
          const a = gen(node[1])
          const b = gen(node[2])
          const c = statement(node[3])
          return `for (let ${a}=0; ${a}<${b}; ++${a}) {${c}}`
        } else {
          fail('Unknown format of for', {node})
        }
      case 'while': return `while (${gen(node.slice(1, -1))}) {${statement(node[node.length - 1])}}`
      case '.': return `(${gen(node[1])}).${node[2]}`
      default:
        if (node[0] === '/') {
          return `(__d => __d == 0 ? error('Zero division error') : ${gen(node[1])} / __d)(${gen(node[2])})`
        } else if (isOp2(node[0])) {
          return gen(node[1]) + node[0] + gen(node[2])
        } else {
          return gen(node[0]) + (node.length === 1 ? '' : `(${node.slice(1).map(gen)})`)
        }
    }
  }
  return nodes.map(gen).join('\n')
}
const run = (src, stdin) => {
  const tokens = tokenize(src)
  const nodes = parse(tokens)
  const js = generate(nodes)

  const stdlib = `let __stdout = ''
const error = msg => { throw new Error(msg) }
const io = {
  print: o => __stdout += o.toString() + '\\n',
  stdin: ${str(stdin)},
}`

  let result
  try {
    result = Function(stdlib + '\n' + js + '\nreturn {ret: main(), stdout: __stdout}')()
  } catch(e) {
    result = {ret: e.message, stdout: ''}
  }
  return {ret: result.ret, stdout: result.stdout, tokens, nodes, js}
}

const test = () => {
  const exp = (expect, exp, ...defs) => test(o => o.ret, '', expect, exp, ...defs)
  const stdin = (expect, stdin, exp, ...defs) => test(o => o.ret, stdin, expect, exp, ...defs)
  const stdout = (expect, exp, ...defs) => test(o => o.stdout, '', expect, exp, ...defs)
  const test = (f, stdin, expect, exp, ...defs) => {
    const src = defs.concat([`def main: ${exp}`]).join('\n')
    const result = run(src, stdin)
    if (eq(expect, f(result))) {
      process.stdout.write('.')
    } else {
      puts('src:', src)
      puts('js:', result.js)
      puts('expect:', expect)
      puts('return:', result.ret)
      puts('tokens:', result.tokens)
      dump(result.nodes)
      process.exit(1)
    }
  }

  // primitives
  exp(1, '1')
  exp('hi', '"hi"')
  exp([1, 2], 'array 1 2')
  exp({1: 2, 3: 4}, 'dict 1 2 1+2 1+3')

  // exp
  exp(3, '1 + 2')
  exp(7, '1 + 2 * 3')
  exp(5, '1 * 2 + 3')

  // function
  exp(3, 'add 1 2', 'def add a b: a + b')
  exp(6, 'calc 2 3', 'def calc a b:\n  def mul a b: a * b\n  mul a b')

  // struct
  exp({x:1, y:2}, 'vector2 1 2', 'struct vector2:\n  x int\n  y int')
  exp(2, '(vector2 1 2).y', 'struct vector2:\n  x int\n  y int')

  // constant
  exp(2, '\n  let a inc 1\n  a', 'def inc a: a + 1')

  // variable
  exp(3, '\n  var a 1\n  a += 2\n  a')

  // branch
  exp(1, 'if true 1 2')
  exp(2, 'if false 1 2')
  exp(2, 'if (true && (1 == 2)) 1 2')

  // for block
  exp(3, '\n  var n 0\n  for i 3: n+=1\n  n')

  // while block
  exp(3, '\n  var n 0\n  while n < 3: n+=1\n  n')

  // branch block
  exp(3, '\n  var n 0\n  if true:\n    n+=1\n    n+=2\n  n')

  // error handling
  exp('Zero division error', '\n  1/0\n  1')
  exp('error', '\n  error "error"\n  1')

  // stdio
  stdin('standard input', 'standard input', 'io.stdin')
  stdout('hello\nworld\n', '\n  io.print "hello"\n  io.print "world"')

  // API for int
  // API for string
  // API for array
  // API for dictionary
  // API for error

  puts('ok')
}

if (module.parent) {
  const src = require('fs').readFileSync('/dev/stdin', 'utf8').trim()
  const tokens = tokenize(src)
  const nodes = parse(tokens)
  const js = generate(nodes)
  puts(js + '\nmain()')
} else {
  test()
}
