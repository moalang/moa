'use strict'
const util = require('util')
const print = (...a) => (console.log(...a), a[0])
const log = (...a) => (console.error(...a.map(o => util.inspect(o, false, null, true))), a[0])
const attempt = f => { try { return f() } catch (e) { return e } }

const execute = (source, embedded) => {
  // parser
  let offset = 0
  const tokens = source.split(/([()]|\s+)/).flatMap(code => {
    offset += code.length
    return code.trim() ? [{code, offset}] : []
  })
  let pos = 0
  // TODO: syntax desugar
  // - [x] a + b     -> (+ a b)
  // - [x] a + b * c -> (+ a (* b c))
  // - [x] a b       -> (a b)
  // - [ ] a b\nc d  -> (a b) (c d)
  // - [ ] a b; c d  -> (a b) (c d)
  const op2s = '|| && == != < <= > >= + - * / % ^ **'.split(' ') // low...high, other operators are lowest
  const priority = t => op2s.findIndex(op => op === t.code)
  const isOp2 = t => t && /^[+\-*\/%<>!=~.]/.test(t.code)
  const reorder = (a, b) => a.length <= 2 ? b.concat(a) :
    !isOp2(a[1]) ? reorder(a.slice(1), b.concat(a.slice(0, 1))) :
    Array.isArray(a[0]) && isOp2(a[0][0]) && priority(a[1]) > priority(a[0][0]) ?
    reorder([[a[0][0], a[0][1], [a[1], a[0][2], a[2]]]].concat(a.slice(3)), b) :
    reorder([[a[1], a[0], a[2]]].concat(a.slice(3)), b)
  const list = a => (t => t.code === ')' ? a : list(a.concat([t])))(unit())
  const unit = () => (t => t.code === '(' ? reorder(list([]), []) : t)(tokens[pos++])
  const nodes = []
  while (pos < tokens.length) {
    nodes.push(unit())
  }

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
