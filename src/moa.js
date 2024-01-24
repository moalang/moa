'use strict'
const util = require('util')
const print = (...a) => (console.log(...a), a[0])
const log = (...a) => (console.error(...a.map(o => util.inspect(o, false, null, true))), a[0])
const attempt = f => { try { return f() } catch (e) { return e } }
const until = (f, g) => { const a = []; while (f()) { a.push(g()) }; return a }

const execute = (source, embedded) => {
  // parser
  let offset = 0
  let lineno = 1
  const tokens = source.split(/([();]|\s+)/).flatMap(code => {
    offset += code.length
    lineno += code.split('\n').length - 1
    return code.trim() ? [{code, lineno, offset}] : []
  })
  let pos = 0
  // TODO: syntax desugar
  // - [x] a + b     -> (+ a b)
  // - [x] a + b * c -> (+ a (* b c))
  // - [x] a b       -> (a b)
  // - [x] a b\nc d  -> (a b) (c d)
  // - [x] a b; c d  -> (a b) (c d)
  // - [ ] {a}       -> {a}
  // - [ ] {a; b c}  -> {a (b c)}
  // - [ ] a()       -> (a)
  // - [ ] a(b)      -> (a b)
  // - [ ] a.b()     -> ((. a b))
  // - [ ] a.b(c)    -> ((. a b) c)
  const op2s = '|| && == != < <= > >= + - * / % ^ **'.split(' ') // low...high, other operators are lowest
  const priority = t => op2s.findIndex(op => op === t.code)
  const isOp2 = t => t && /^[+\-*\/%<>!=~.]/.test(t.code)
  const reorder = xs => {
    const stack = []
    for (let i=0; i<xs.length; i++) {
      const x = xs[i]
      if (isOp2(x) && stack.length) {
        const prev = stack.at(-1)
        if (isOp2(prev[0]) && priority(prev[0]) < priority(x)) {
          const [op, lhs, rhs] = prev
          stack[stack.length - 1] = [op, lhs, [x, rhs, xs[++i]]]
        } else {
          stack[stack.length - 1] = [x, prev, xs[++i]]
        }
      } else {
        stack.push(x)
      }
    }
    return xs.length === 3 && stack.length === 1 ? stack[0] : stack
  }
  const list = a => (t => t.code === ')' ? a : list(a.concat([t])))(unit())
  const unit = () => (t => t.code === '(' ? reorder(list([])) : t)(tokens[pos++])
  const line = l => {
    const a = until(() => pos < tokens.length && tokens[pos].lineno === l && tokens[pos].code !== ';', unit)
    return a.length === 1 ? a[0] : reorder(a)
  }
  const top = () => (t => t.code === '(' ? unit() :
    t.code === ';' ? (++pos, line(t.lineno)) :
    line(t.lineno))(tokens[pos])
  const nodes = until(() => pos < tokens.length, top)

  // interpriter
  const fail = (m, o) => { log(o); throw new Error(m) }
  const call = (env, f, a) => typeof f === 'function' ? f(env, a) : fail('NotFunction', {f, a})
  const run = (env, target) =>
    Array.isArray(target) ? call(env, run(env, target[0]), target.slice(1)) :
    target.code === 'true' ? true :
    target.code === 'false' ? false :
    target.code.match(/^[0-9]/) ? parseFloat(target.code) :
    target.code.match(/^["'`]/) ? c.slice(1, -1) :
    target.code in env ? env[target.code] :
    fail('Missing', {target, ids: [...Object.keys(env)]})
  const dict = (a, b) => Object.fromEntries(a.map((x,i) => [x,b[i]]))
  Object.assign(embedded, {
    def: (env, [name, a, body]) => env[name.code] = (e, b) =>
      run({...e, ...dict(a.map(x => x.code), b.map(exp => run(e, exp)))}, body),
    var: (env, [name, exp]) => env[name.code] = run(env, exp),
    let: (env, [name, exp]) => env[name.code] = run(env, exp),
    struct: (env, [name, fields]) => env[name.code] = (e, a) =>
      dict(fields.map(f => f[0].code), a.map(exp => run(e, exp))),
    log: (env, a) => log(...a.map(exp => run(env, exp))),
    print: (env, a) => print(...a.map(exp => run(env, exp))),
    '.': (env, [obj, name]) => run(env, obj)[name.code],
  }); // semi-corron is needed here
  [
    ['+', (l, r) => l + r],
    ['-', (l, r) => l - r],
    ['*', (l, r) => l * r],
    ['/', (l, r) => l / r],
    ['%', (l, r) => l % r],
    ['||', (l, r) => l || r],
    ['&&', (l, r) => l && r],
    ['**', (l, r) => l ** r],
  ].map(([op, opf]) => {
    embedded[op] = (env, [head, ...a]) => a.reduce((acc, x) => opf(acc, run(env, x)) , run(env, head)),
    embedded[op + '='] = (env, [l, r]) => env[l.code] = opf(run(env, l), run(env, r))
  })
  return nodes.map(node => run(embedded, node)).at(-1)
}

const repl = () => {
  const rl = require('node:readline/promises').createInterface({
    input: process.stdin,
    output: process.stdout,
  })
  process.stdout.write('> ')
  const env = {}
  rl.on('line', line => {
    if ('exit quit q'.split(' ').includes(line)) {
      log('Bye \u{1F44B}')
      rl.close()
    } else {
      log(attempt(() => execute(line, env)))
      process.stdout.write('> ')
    }
  })
}

const fs = require('fs')
if (process.stdin.isRaw === undefined) {
  execute(fs.readFileSync('/dev/stdin', 'utf8'), {})
} else {
  repl()
}
