const log = (...a) => (console.log(...a), a[0])

const run = source => {
  // parser
  let offset = 0
  const simplify = o => Array.isArray(o) ?  o.map(simplify) : o.code
  const show = o => log(JSON.stringify(simplify(o), null, 2))
  const tokens = source.split(/([()]|\s+)/).flatMap(code => {
    offset += code.length
    return code.trim() ? [{code, offset}] : []
  })
  let pos = 0
  const list = l => (t => t.code === ')' ? (l) : list(l.concat([t])))(unit())
  const unit = () => (t => t.code === '(' ? list([]) : t)(tokens[pos++])
  const top = []
  while (pos < tokens.length) {
    top.push(unit())
  }

  // interpriter
  const fail = (m, o) => { console.dir(o, {depth: null}); throw new Error(m) }
  const dict = (a, b) => Object.fromEntries(a.map((x,i) => [x,b[i]]))
  const macro = {
    'def': (env, name, a, exp) => env[name.code] = (...b) => evaluate(exp, {...env, ...dict(a.map(x => x.code), b)}),
    'var': (env, name, exp) => env[name.code] = evaluate(exp, env),
    '+=': (env, l, r) => env[l.code] = evaluate(l, env) + evaluate(r, env),
    '-=': (env, l, r) => env[l.code] = evaluate(l, env) - evaluate(r, env),
    '*=': (env, l, r) => env[l.code] = evaluate(l, env) * evaluate(r, env),
    '/=': (env, l, r) => env[l.code] = evaluate(l, env) / evaluate(r, env),
    '%=': (env, l, r) => env[l.code] = evaluate(l, env) % evaluate(r, env),
  }
  const evaluate = (target, env) => {
    const run = x => evaluate(x, env)
    if (Array.isArray(target)) {
      const head = target[0].code
      return head in macro ? macro[head](env, ...target.slice(1)) :
        run(target[0])(...target.slice(1).map(run))
    } else {
      const c = target.code
      return c === 'true' ? true :
        c === 'false' ? false :
        c.match(/^[0-9]/) ? parseFloat(c) :
        c.match(/^["'`]/) ? c.slice(1, -1) :
        c in env ? env[c] :
        fail('Missing', {target, ids: [...Object.keys(env)]})
    }
  }
  const embedded = {
    '+': (l, r) => l + r,
    '-': (l, r) => l - r,
    '*': (l, r) => l * r,
    '/': (l, r) => l / r,
    '%': (l, r) => l % r,
    log: (...a) => log(...a),
  }
  try {
    top.map(node => evaluate(node, embedded))
  } catch (e) {
    log(e.message + e.stack.split('\n')[2].match(/(:\d+):\d/)[1])
    process.exit(1)
  }
  return 'console.log(123)'
}

const repl = () => {
  const rl = require('node:readline/promises').createInterface({
    input: process.stdin,
    output: process.stdout,
  })
  process.stdout.write('> ')
  rl.on('line', line => {
    if ('exit quit q'.split(' ').includes(line)) {
      log('Bye \u{1F44B}')
      rl.close()
    } else {
      eval(line)
      process.stdout.write('> ')
    }
  })
}

const fs = require('fs')
if (process.stdin.isRaw === undefined) {
  run(fs.readFileSync('/dev/stdin', 'utf8'))
} else {
  repl()
}
