'use strict'

const puts = (...a) => console.log(...a)
const dump = o => console.dir(o, {depth: null})
const trace = (...a) => (puts(...a), a[a.length - 1])
const str = JSON.stringify
const eq = (a,b) => str(a) === str(b)
const isArray = o => typeof o === 'object' && o.constructor === Array
const op2Assign = '= += -= *= /= ='.split(' ')
const isAssign = t => op2Assign.includes(t)
const op2 = '+ - * / % += -= *= /= %= == != < > <= >= && ||'.split(' ')
const isOp2 = t => op2.includes(t)
const fail = (msg, o) => {dump(o); throw new Error(msg)}

const tokenize = src => src.split(/([0-9]+|[a-zA-Z_][a-zA-Z_0-9]*(?:,[a-zA-Z_][a-zA-Z_0-9]*)*|[ \n]+|[.+\-*/=!&|>]+|"[^"]*"|`[^"]*`|(?:#.*\n)|.)/).filter(t => t[0] !== '#').map(toToken).filter(t => t)
const toToken = t => t.includes('\n') ? t.slice(t.lastIndexOf('\n')) : t.trim()
const parse = tokens => {
  let pos = 0
  const consume = f => pos < tokens.length ? f(tokens[pos++]) : fail('EOT', {f: f.toString(),tokens})
  const next = v => (++pos, v)
  const many = (f, g, a) => {
    a = a || []
    while (pos < tokens.length && g(tokens[pos])) {
      const t = f(tokens[pos])
      if (t === '=>') {
        a[a.length - 1] = many(f, g, [t, a[a.length - 1]])
      } else if (t === '.') {
        const dot = [t, a[a.length - 1], tokens[pos++]]
        if (isOp2(tokens[pos])) {
          a[a.length - 1] = many(f, g, [tokens[pos++], dot])
        } else if (a.length === 1) {
          a[a.length - 1] = many(f, g, dot)
        } else {
          a[a.length - 1] = dot
        }
      } else if ((isOp2(t) || isAssign(t)) && a.length) {
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
    t === '[' ? next(many(unit, t => t !== ']', ['array'])) :
    t === ':' ? block() :
    t)
  const lines = indent => many(line, t => t === indent && ++pos, [line()])
  const line = () => many(unit, t => t[0] !== '\n')
  return lines('\n')
}
const generate = nodes => {
  const map = a => 'new __map({' + [...Array(a.length / 2).keys()].map(i => i * 2).map(i => `[${a[i]}]:${a[i+1]}`) + '})'
  const addReturn = a => (!a[a.length-1].match(/^if|for/) ? a[a.length-1] = `return ${a[a.length-1]}` : 0,a)
  const statement = a => a.length === 1 ? gen(a[0]) : `{\n  ${a.map(gen).join('  \n')}\n}`
  const exps = a => `{\n  ${addReturn(a.map(gen)).join('\n  ')}\n}`
  const gen = node => {
    if (!isArray(node)) {
      if (node[0].match(/^[a-zA-Z_]/)) {
        return `__ref(${node})`
      } else {
        return node
      }
    }
    switch (node[0]) {
      case 'def': return `const ${node[1]} = (${node.slice(2, -1)}) => ${exps(node[node.length - 1])}`
      case 'struct':
        const names = node[node.length - 1].map(field => field[0])
        return `const ${node[1]} = (${names}) => ({${names}})`
      case 'array': return `[${node.slice(1).map(gen)}]`
      case 'map': return `(${map(node.slice(1).map(gen))})`
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
          const a = node[1]
          const b = gen(node[2])
          const c = statement(node[3])
          return `for (let ${a}=0; ${a}<${b}; ++${a}) {${c}}`
        } else {
          fail('Unknown format of for', {node})
        }
      case 'while': return `while (${gen(node.slice(1, -1))}) {${statement(node[node.length - 1])}}`
      case '.': return `__dot(() => ${gen(node[1])}, '${node[2]}', [${node.slice(3).map(gen)}])`
      default:
        if (node[0] === '/') {
          return `(__d => __d == 0 ? error('Zero division error') : ${gen(node[1])} / __d)(${gen(node[2])})`
        } else if (node[0] === '=>') {
          return `((${node[1]}) => ${gen(node.slice(2))})`
        } else if (node[0] === '-' && node.length === 2) {
          return '-' + gen(node[1])
        } else if (isAssign(node[0])) {
          return node[1] + node[0] + gen(node[2])
        } else if (isOp2(node[0])) {
          return gen(node[1]) + node[0] + gen(node[2])
        } else {
          return gen(node[0]) + (node.length === 1 ? '' : `(${node.slice(1).map(gen)})`)
        }
    }
  }
  return nodes.map(gen).join('\n')
}
const stdlib = stdin => `let __stdout = ''
const error = msg => { throw new Error(msg) }
const io = {
  print: o => __stdout += o.toString() + '\\n',
  stdin: ${str(stdin)},
}
function __map(o) { this.o = o }
__map.prototype = {
  get(k) { return this.o[k] },
  set(k, v) { this.o[k] = v },
  get keys() { return Object.keys(this.o) },
  get values() { return Object.values(this.o) },
}
__map.prototype.constructor = __map
const __unwrap = o => typeof o === 'object' && o.constructor === __map ? o.o : o
const __ref = o => typeof o === 'function' && o.toString().startsWith('()') ? o() : o
const not = o => !o

const __dot = (f, label, args) => {
  const ref = () => {
    if (label === 'then') {
      return g => g(f())
    } else if (label === 'catch') {
      return g => {
        try {
          return f()
        } catch (e) {
          return g(e)
        }
      }
    } else {
      const o = f()
      const t = typeof o
      if (t === 'string') {
        switch (label) {
        case 'size': return o.length
        case 'at': return i => o[i]
        }
      } else if (t === 'number') {
        // no methods
      } else if (t === 'object') {
        if (o.constructor === Array) {
          switch (label) {
          case 'size': return o.length
          case 'map': return o[label](...args)
          case 'filter': return o[label](...args)
          case 'push': return o[label](...args)
          }
        } else if (label in o) {
          if (args.length === 0) {
            return o[label]
          } else {
            return o[label](...args)
          }
        } else {
          error('Unknown field ' + t + ' in ' + JSON.stringify(o))
        }
      }
      error('Unknown reference ' + t + ' with ' + label)
    }
  }
  const o = ref()
  return typeof o === 'function' && args.length ? o(...args) : o 
}
`
const run = (src, stdin) => {
  const tokens = tokenize(src.trim())
  const nodes = parse(tokens)
  const js = generate(nodes)
  const runtime = stdlib(stdin) + js + '\nreturn {ret: __unwrap(main()), stdout: __stdout}'
  let result = {tokens, nodes, js, runtime}
  try {
    return Object.assign(result, Function(runtime)())
  } catch(e) {
    return Object.assign(result, {ret: e.message, stdout: '', error: e})
  }
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
  exp([1, 2], '[1 2]')
  exp({1: 2, 3: 4}, 'map 1 2 1+2 1+3')
  exp(1, '(n => n) 1')
  exp(3, '(a,b => a + b) 1 2')
  exp(6, '(a,b,c => a + b + c) 1 2 3')

  // exp
  exp(3, '1 + 2')
  exp(7, '1 + 2 * 3')
  exp(5, '1 * 2 + 3')
  exp(true, '[1 2].size == [3 4].size')
  exp(1, '\n  var n 0\n  n = 1\n  n')

  // function
  exp(1, 'one', 'def one: 1')
  exp(3, 'add 1 2', 'def add a b: a + b')
  exp(6, 'calc 2 3', 'def calc a b:\n  def mul a b: a * b\n  mul a b')

  // struct
  exp({x:1, y:2}, 'vector2 1 2', 'struct vector2:\n  x int\n  y int')
  exp(2, '(vector2 1 2).y', 'struct vector2:\n  x int\n  y int')

  // constant
  exp(2, '\n  let a inc 1\n  a', 'def inc a: a + 1')

  // variable
  exp(3, '\n  var a 1\n  a += 2\n  a')
  exp(3, '\n  var a 1\n  def inc: a += 1\n  inc\n  inc\n  a')

  // branch
  exp(1, 'if true 1 2')
  exp(2, 'if false 1 2')
  exp(2, 'if (true && (1 == 2)) 1 2')

  // for block
  exp(3, '\n  var n 0\n  for i 3: n+=1\n  n')
  exp(2, '\n  var n 0\n  for i [1 2].size: n+=1\n  n')

  // while block
  exp(3, '\n  var n 0\n  while n < 3: n+=1\n  n')

  // branch block
  exp(3, '\n  var n 0\n  if true:\n    n+=1\n    n+=2\n  n')

  // error handling
  exp('Zero division error', '\n  1/0\n  1')
  exp('error', '\n  error "error"\n  1')
  exp(3, '1.then(n => n + 2)')
  exp(1, '1.catch(n => n + 2)')
  exp('fail', '(error "fail").then(n => n + 2)')
  exp('ok', '(error "fail").catch(e => "ok")')

  // stdio
  stdin('standard input', 'standard input', 'io.stdin')
  stdout('hello\nworld\n', '\n  io.print "hello"\n  io.print "world"')

  // int
  exp(-1, '(-1)')
  exp(0, '-1 + 1')
  exp(0, 'add 1 (-1)', 'def add a b: a + b')

  // string
  exp(2, '"hi".size')
  exp('i', '"hi".at(1)')

  // array
  exp(2, '[1 2].size')
  exp([2, 3], '[1 2].map(n => n + 1)')
  exp([1, 3], '[1 2 3].filter(n => n % 2 == 1)')

  // map
  exp(1, '(map "a" 1 "b" 2).get "a"')
  exp(2, '\n  var d map\n  d.set 1 2\n  d.get 1')
  exp(['a', 'b'], '(map "a" 1 "b" 2).keys')
  exp([1, 2], '(map "a" 1 "b" 2).values')

  // comment
  exp(1, 'one', '# comment', 'def one: 1')

  puts('ok')
}

if (module.parent) {
  const fs = require('fs')
  const src = fs.readFileSync(process.argv[2], 'utf8')
  const result = run(src, src)
  process.stdout.write(result.stdout)
  if (result.error) {
    console.error(result.error)
    console.log('--')
    console.log(result.runtime)
    process.exit(1)
  }
} else {
  test()
}
