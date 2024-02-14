'use strict'

const fs = require('node:fs')
const util = require('node:util')
const log = (...a) => (console.error(...a.map(o => typeof o === 'function' ? o.toString() : util.inspect(o, false, null, true))), a[0])
const fail = (m, o) => { const e = new Error(m); e.detail = o; throw e }

const parse = source => {
  let offset = 0
  // operator | symbols | id | number | string | white space
  const tokens = source.split(/([+\-*\/%|&<>!=^]+|[()\[\]]|[A-Za-z_][0-9A-Za-z_]*|[0-9]+(?:\.[0-9]+)?|".*?(?<!\\)"|(?:#[^\n]*|\s+))/s).flatMap(code => {
    offset += code.length
    return code.match(/^[^#\s]/) ? [{code, offset}] : []
  })
  let pos = 0
  const many = (f, acc) => pos >= tokens.length ? acc :
    ((t, acc) => t ? many(f, acc.concat([t])) : acc)(f(tokens[pos++]), acc)
  const list = () => suffix(many(t => t.code !== ')' && t.code !== ']' && unit(t), []))
  const unit = t => t.code === '(' ? list() :
    t.code === '!' ? suffix([t, unit(tokens[pos++])]) :
    t.code === '-' && tokens[pos-2] && tokens[pos-2].code !== '(' ? suffix([t, unit(tokens[pos++])]) :
    suffix(t)
  const suffix = t => {
    if (tokens.length <= pos) {
      return t
    }
    const tt = tokens[pos++]
    const near = tokens[pos-2].offset === tt.offset - tt.code.length
    return tt.code === '.' ? suffix([tt, t, tokens[pos++]]) :
      near && tt.code === '(' ? suffix([t, ...list()]) :
      near && tt.code === '[' ? suffix([tt, t, ...list()]) :
      (--pos, t)
  }
  return many(unit, [])
}

const execute = (env, node) => {
  const qmap = {'r': '\r', 'n': '\n', 't': '\t'}
  const unquote = s => s.replace(/\\(.)/g, (_, c) => qmap[c] || c)
  const trap = (target, f )=> { try { return f() } catch (e) { e.target = target; throw e } }
  const run = target => trap(target, () =>
    Array.isArray(target) ? run(target[0])(env, ...target.slice(1)) :
    target.code === 'true' ? true :
    target.code === 'false' ? false :
    target.code.match(/^[0-9]/) ? parseFloat(target.code) :
    target.code.match(/^["'`]/) ? unquote(target.code.slice(1, -1)) :
    env.has(target.code) ? env.get(target.code) :
    fail(`not find value \`${target.code}\` in this scope`, {target, ids: env.keys()}))
  return run(node)
}

const newEnv = () => {
  class Return { constructor(v) { this.v = v } valueOf() { return this.v } }
  const attempt = (f, g) => { try { return f() } catch (e) { return g ? g(e) : e } }
  const compare = (a, b, f) =>
    a === undefined ? b === undefined :
    a === null ? b === null :
    a.constructor.name !== b.constructor.name ? fail(`${a.constructor.name} and ${b.constructor.name} are can not be compared by ${f.toString()}`) :
    a.constructor === String  ? f(a, b) :
    a.constructor === Number  ? f(a, b) :
    a.constructor === Boolean ? f(a, b) :
    a.constructor === Error   ? f(a.message, b.message) :
    a.constructor === Array   ? a.length === b.length && a.every((x,i) => compare(x, b[i], f)) :
    a.constructor === Map     ? [...a.keys()].sort().map(k => compare(a.get(k), b.get(k), f)) :
    a.constructor === Object  ? Object.keys(a).sort().every(k => compare(a[k], b[k], f)) :
    fail(`${a} and ${b} are not compared by ${f.toString()}`)
  const properties = {
    'String size': s => s.length,
  }
  const property = (key, x) => key in properties ? properties[key](x) : fail(`not find property \`${key}\` in \`${x}\``)
  const assign = (f, x, v) => !Array.isArray(x) ? f.set(x.code, v) :
    x[0].code === '[' ? f.get(x[1].code)[f(x[2])] = v :
    x[0].code === '.' ? f.get(x[1].code)[x[2].code] = v :
    fail('assign', x)
  const env = {
    assert: (f, l, r) => compare(f(l), f(r), (a, b) => a === b || fail('assert', {l, r, a, b})),
    do: (f, ...a) => (f => a.reduce((acc, x) => acc instanceof Return ? acc : f(x), '').valueOf())(f.with({})),
    fn: (_, ...a) => (f, ...b) => f.with(Object.fromEntries(a.slice(0, -1).map((x, i) => [x.code, f(b[i])])))(a.at(-1)),
    var: (f, k, v) => f.put(k.code, f(v)),
    list: (f, ...a) => a.map(f),
    '[': (f, a, i) => f(a)[f(i)],
    '!': (f, x) => !f(x),
    '.': (f, t, u) => (x => property(`${x.constructor.name} ${u.code}`, x))(f(t)),
    '=': (f, l, r) => assign(f, l, f(r)),
    '==': (f, l, r) => f(l) == f(r),
    '!=': (f, l, r) => f(l) != f(r),
    '>': (f, l, r) => f(l) >  f(r),
    '>=': (f, l, r) => f(l) >= f(r),
    '<': (f, l, r) => f(l) <  f(r),
    '<=': (f, l, r) => f(l) <= f(r),
    '++': (f, l, r) => f(l).concat(f(r)),
  }
  const op2s = {
    '||': (f, l, r) => f(l) || f(r),
    '&&': (f, l, r) => f(l) && f(r),
    '+': (f, l, r) => f(l) + f(r),
    '-': (f, l, r) => r === undefined ? -f(l) : f(l) - f(r),
    '*': (f, l, r) => f(l) * f(r),
    '/': (f, l, r) => f(l) / f(r),
    '%': (f, l, r) => f(l) % f(r),
    '|': (f, l, r) => f(l) | f(r),
    '&': (f, l, r) => f(l) & f(r),
    '^': (f, l, r) => f(l) ^ f(r),
    '*': (f, l, r) => f(l) * f(r),
    '**': (f, l, r) => f(l) ** f(r),
  }
  const updates = Object.fromEntries(Object.keys(op2s).map(op => [op + '=', (f, l, r) => f.set(l.code, op2s[op](f, l, r))]))
  Object.assign(env, op2s, updates)
  const f = m => {
    const g = node => execute(g, node)
    g.has = key => key in m || key in env
    g.get = key => (key in m ? m : env)[key].valueOf()
    g.set = (key, val) => m[key].val = val
    g.put = (key, val) => m[key] = {valueOf() { return val }}
    g.keys = () => Object.keys(m)
    g.with = obj => f({...m, ...obj})
    return g
  }
  return f({})
}

const env = newEnv({})
for (const chunk of fs.readFileSync('/dev/stdin', 'utf8').split(/^(?=\()/m)) {
  for (const node of parse(chunk)) {
    try {
      execute(env, node)
      process.stdout.write('.')
    } catch(e) {
      log(e)
      console.log(`echo ${JSON.stringify(chunk.trim())} | node src/moa.js`)
      process.exit(1)
    }
  }
}
