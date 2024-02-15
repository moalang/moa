'use strict'

const fs = require('node:fs')
const child_process = require('node:child_process')
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
  const list = () => many(t => t.code !== ')' && t.code !== ']' && unit(t), [])
  const unit = t => t.code === '(' ? suffix(list()) :
    t.code === '!' ? suffix([t, unit(tokens[pos++])]) :
    t.code === '-' && tokens[pos-2] && tokens[pos-2].code !== '(' ? suffix([t, unit(tokens[pos++])]) :
    suffix(t)
  const suffix = t => {
    if (tokens.length <= pos) {
      return t
    }
    const tt = tokens[pos++]
    const near = tt.offset - tt.code.length === tokens[pos-2].offset
    return tt.code === '.' ? suffix([tt, t, tokens[pos++]]) :
      near && tt.code === '(' ? suffix([t, ...list()]) :
      near && tt.code === '[' ? suffix([tt, t, ...list()]) :
      (--pos, t)
  }
  return many(unit, [])
}

class Record { constructor(v) { this.v = v } }

const execute = (env, node) => {
  const qmap = {'r': '\r', 'n': '\n', 't': '\t'}
  const unquote = s => s.replace(/\\(.)/g, (_, c) => qmap[c] || c)
  const trap = (target, f )=> { try { return f() } catch (e) { e.target = target; throw e } }
  const run = target => trap(target, () =>
    target instanceof Record ? target :
    Array.isArray(target) ? env.call(run(target[0]), ...target.slice(1)) :
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
  const eq = (l, r) => compare(l, r, (a, b) => a === b)
  const properties = {
    'String size': s => s.length,
  }
  const property = (x, field, key) => key in properties ? properties[key](x) :
    x instanceof Record && field in x.v ? x.v[field] :
    fail(`not find property \`${key}\` in \`${x}\``)
  const assign = (f, x, v) => !Array.isArray(x) ? f.set(x.code, v) :
    x[0].code === '[' ? f.get(x[1].code)[f(x[2])] = v :
    x[0].code === '.' ? f.get(x[1].code)[x[2].code] = v :
    fail('assign', x)
  const iif = (f, [a, b, ...c]) => b === undefined ? f(a) : f(a) ? f(b) : iif(f, c)
  const case_ = (f, a, [b, c, ...d]) => b.code === '_' || eq(a, f(b)) ? f(c) : case_(f, a, d)
  const lazy = f => (f.lazy = true, f)
  const laziness = {
    do: (f, ...a) => (f => a.reduce((acc, x) => acc instanceof Return ? acc : f(x), '').valueOf())(f.with({})),
    fn: (f, ...a) => (...b) => f.with(Object.fromEntries(a.slice(0, -1).map((x, i) => [x.code, b[i]])))(a.at(-1)),
    var: (f, k, v) => f.put(k.code, f(v)),
    let: (f, k, v) => f.put(k.code, f(v)),
    struct: (f, ...a) => new Record(Object.fromEntries([...Array(a.length/2)].map((_,i) => [a[i*2].code, f(a[i*2+1])]))),
    '.': (f, t, u) => (x => property(x, u.code, `${x.constructor.name} ${u.code}`))(f(t)),
    '=': (f, l, r) => assign(f, l, f(r)),
    iif: (f, ...a) => iif(f, a),
    case: (f, ...a) => case_(f, f(a[0]), a.slice(1)),
    catch: (f, a, b) => attempt(() => f(a), e => f.call(f(b), new Record(e))),
  }
  const embedded = {
    ...Object.fromEntries(Object.keys(laziness).map(k => [k, lazy(laziness[k])])),
    assert: (l, r) => compare(l, r, (a, b) => a === b || fail('assert', {l, r, a, b})),
    list: (...a) => a,
    dict: (...a) => new Map([...Array(a.length/2)].map((_,i) => [a[i*2].code, a[i*2+1]])),
    io: new Record({
      fs: new Record({
        write: (path, x) => (fs.writeFileSync(path, x), x.length),
        reads: path => fs.readFileSync(path, 'utf8'),
        rm: path => (fs.unlinkSync(path), true),
      }),
      shell: (...a) => {
        const result = child_process.execSync(a.join(' '), {encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore']})
        return new Record({result})
      },
    }),
    '[': (a, i) => a[i],
    '!': (x) => !x,
    '>': (l, r) => l > r,
    '<': (l, r) => l < r,
    '==': (l, r) => l == r,
    '!=': (l, r) => l != r,
    '>=': (l, r) => l >= r,
    '<=': (l, r) => l <= r,
  }
  const op2s = {
    '+': (l, r) => l + r,
    '-': (l, r) => r === undefined ? -l : l - r,
    '*': (l, r) => l * r,
    '/': (l, r) => l / r,
    '%': (l, r) => l % r,
    '|': (l, r) => l | r,
    '&': (l, r) => l & r,
    '^': (l, r) => l ^ r,
    '*': (l, r) => l * r,
    '**': (l, r) => l ** r,
    '||': (l, r) => l || r,
    '&&': (l, r) => l && r,
    '++': (l, r) => l instanceof Map ? new Map([...l, ...r]) : l.concat(r),
  }
  const updates = Object.fromEntries(Object.keys(op2s).map(op => [op + '=', lazy((f, l, r) => f.set(l.code, op2s[op](f(l), f(r))))]))
  Object.assign(embedded, op2s, updates)

  const f = m => {
    const g = node => execute(g, node)
    g.has = key => key in m || key in embedded
    g.get = key => key in m ? m[key].val : embedded[key]
    g.set = (key, val) => m[key].val = val
    g.put = (key, val) => (m[key] = {val}, val)
    g.keys = () => Object.keys(m)
    g.with = obj => (Object.keys(obj).map(key => obj[key] = {val: obj[key]}), f({...m, ...obj}))
    g.call = (h, ...a) => h.lazy ? h(g, ...a) : h(...a.map(g))
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
      console.log(`echo '${chunk.trim().replace(/'/g, "`")}' | node src/moa.js`)
      process.exit(1)
    }
  }
}
