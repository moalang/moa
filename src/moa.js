'use strict'
const print = (...a) => (console.log(...a), a[0])
const log = (...a) => (console.error(...a), a[0])

const execute = (source, embedded) => {
  // parser
  let offset = 0
  const simplify = o => Array.isArray(o) ?  o.map(simplify) : o.code
  const show = o => log(JSON.stringify(simplify(o), null, 2))
  const tokens = source.split(/([()]|\s+)/).flatMap(code => {
    offset += code.length
    return code.trim() ? [{code, offset}] : []
  })
  let pos = 0
  // TODO: syntax desugar
  // - [ ] a b      -> (a b)
  // - [ ] a b\nc d -> (a b) (c d)
  // - [ ] a b; c d -> (a b) (c d)
  const list = l => (t => t.code === ')' ? (l) : list(l.concat([t])))(unit())
  const unit = () => (t => t.code === '(' ? list([]) : t)(tokens[pos++])
  const top = []
  while (pos < tokens.length) {
    top.push(unit())
  }

  // interpriter
  const fail = (m, o) => { log(o); throw new Error(m) }
  const run = (env, target) =>
    Array.isArray(target) ? run(env, target[0])(env, target.slice(1)) :
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
  ].map(([op, opf]) => {
    embedded[op] = (env, [head, ...a]) => a.reduce((acc, x) => opf(acc, run(env, x)) , run(env, head)),
    embedded[op + '='] = (env, [l, r]) => env[l.code] = opf(run(env, l), run(env, r))
  })
  try {
    return top.map(node => run(embedded, node)).at(-1)
  } catch (e) {
    log(e.stack)
    process.exit(1)
  }
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
      log(execute(line, env))
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
