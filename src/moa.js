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
  class Tuple extends Array {}
  class Some { constructor(v) { this.v = v } valueOf() { return this.v } }
  class None { }
  class Continue { }
  class Break { }

  const none = new None()
  const attempt = (f, g) => { try { return f() } catch (e) { return g ? g(e) : e } }
  const compare = (a, b, f) =>
    a.constructor.name !== b.constructor.name ? fail(`${a.constructor.name} and ${b.constructor.name} are can not be compared by ${f.toString()}`) :
    typeof a === 'string'  ? f(a, b) :
    typeof a === 'boolean' ? f(a, b) :
    typeof a === 'number'  ? f(a, b) :
    typeof a == 'string'   ? f(a, b) :
    a instanceof Error     ? f(a.message, b.message) :
    a instanceof Array     ? a.length === b.length && a.every((x,i) => compare(x, b[i], f)) :
    a instanceof Map       ? [...a.keys()].sort().map(k => compare(a.get(k), b.get(k), f)) :
    a instanceof Object    ? Object.keys(a).sort().every(k => compare(a[k], b[k], f)) :
    fail(`${a} and ${b} are not compared by ${f.toString()}`)
  const eq = (l, r) => compare(l, r, (a, b) => a === b)
  const properties = {
    'String size': s => s.length,
    'String slice': s => (...a) => s.slice(...a),
    'String split': s => (...a) => s.split(...a),
    'String reverse': s => s.split('').reverse().join(''),
    'String replace': s => (...a) => s.replaceAll(...a),
    'String index': s => t => (n => n === -1 ? none : new Some(n))(s.indexOf(t)),
    'String starts': s => t => s.startsWith(t),
    'String ends': s => t => s.endsWith(t),
    'String trim': s => s.trim(),
    'String has': s => t => s.includes(t),
    'RegExp match': r => s => r.test(s),
    'RegExp capture': r => s => (a => a === null ? [] : a)(s.match(r)),
    'RegExp split': r => s => s.split(r),
    'RegExp replace': r => (s, f) => s.replace(r, f),
    'Array at': a => i => a.at(i),
    'Array tie': a => (i, v) => a[i < 0 ? a.length + i : i] = v,
    'Array push': a => v => a.push(v),
    'Array size': a => a.length,
    'Array slice': a => (...b) => a.slice(...b),
    'Array reverse': a => a.reverse(),
    'Array get': a => i => 0 <= i && i < a.length ? new Some(a[i]) : none,
    'Array set': a => (i, v) => 0 <= i && i < a.length ? (a[i] = v, true) : false,
    'Array map': a => f => a.map(f),
    'Array mapi': a => f => a.map(f),
    'Array fmap': a => f => a.flatMap(f),
    'Array keep': a => f => a.filter(f),
    'Array all': a => f => a.every(f),
    'Array any': a => f => a.some(f),
    'Array sort': a => f => f ? a.toSorted(f) : a.toSorted(),
    'Array zip': a => b => a.map((x, i) => new Tuple().concat(x, b[i])),
    'Array fold': a => (v, f) => a.reduce(f, v),
    'Array find': a => f => (r => r === undefined ? none : new Some(r))(a.find(x => f(x))),
    'Array index': a => f => (i => i === -1 ? none : new Some(i))(a.findIndex(x => f(x))),
    'Array join': a => s => a.join(s),
    'Array has': a => x => a.includes(x),
    'Array min': a => Math.min(...a),
    'Array max': a => Math.max(...a),
    'Map size': m => m.size,
    'Map get': m => k => m.has(k) ? new Some(m.get(k)) : none,
    'Map set': m => (k, v) => (b => (m.set(k, v), !b))(m.has(k)),
    'Map has': m => k => m.has(k),
    'Map keys': m => [...m.keys()],
    'Map values': m => [...m.values()],
    'Map list': m => [...m].map(([k,v]) => new Tuple().concat(k, v)),
    'Set has': s => x => s.has(x),
    'Set add': s => x => s.has(x) ? false : (s.add(x), true),
    'Set rid': s => x => s.delete(x),
    'Set list': s => [...s],
    'Number abs': n => Math.abs(n),
    'Number neg': n => -n,
    'Number char': n => String.fromCharCode(n),
    'Number floor': n => Math.floor(n),
    'Number ceil': n => Math.ceil(n),
    'Number round': n => Math.round(n),
    'Some bool': s => true,
    'Some and': s => f => new Some(f(s.v)),
    'Some or': s => () => s.v,
    'None bool': s => false,
    'None and': s => () => none,
    'None or': s => x => x,
  }
  const property = (x, field, key) => key in properties ? properties[key](x) :
    x instanceof Record && field in x.v ? x.v[field] :
    x instanceof Array ? x[field] :
    fail(`not find property \`${key}\` in \`${x}\``)
  const assign = (f, x, v) => !Array.isArray(x) ? f.set(x.code, v) :
    x[0].code === '[' ? f.get(x[1].code)[f(x[2])] = v :
    x[0].code === '.' ? f.get(x[1].code)[x[2].code] = v :
    fail('assign', x)
  const iif = (f, [a, b, ...c]) => b === undefined ? f(a) : f(a) ? f(b) : iif(f, c)
  const case_ = (f, a, [b, c, ...d]) => b.code === '_' || eq(a, f(b)) ? f(c) : case_(f, a, d)
  const fn = (f, ...a) => (...b) => f.with(Object.fromEntries(a.slice(0, -1).map((x, i) => [x.code, b[i]])))(a.at(-1))
  const discontinued = x => x instanceof Return || x instanceof Continue || x instanceof Break
  const statement = (f, a) => a.reduce((acc, x) => discontinued(acc) ? acc : f(x), '__statement')
  const next = (f, cond, body, x) => x instanceof Return ? x :
    x instanceof Break ? '__break' :
    f(cond) && next(f, cond, body, statement(f, body))
  const laziness = {
    fn,
    do: (f, ...a) => statement(f.with({}), a).valueOf(),
    fn: (f, ...a) => (...b) => f.with(Object.fromEntries(a.slice(0, -1).map((x, i) => [x.code, b[i]])))(a.at(-1)),
    def: (f, a, ...b) => f.put(a.code, fn(f, ...b)),
    var: (f, k, v) => f.put(k.code, f(v)),
    let: (f, k, v) => f.put(k.code, f(v)),
    struct: (f, ...a) => new Record(Object.fromEntries([...Array(a.length/2)].map((_,i) => [a[i*2].code, f(a[i*2+1])]))),
    record: (f, a, ...b) => f.put(a.code, (...c) => new Record(Object.fromEntries([...Array(b.length/2)].map((_,i) => [b[i*2].code, c[i]])))),
    '.': (f, t, u) => (x => property(x, u.code, `${x.constructor.name} ${u.code}`))(f(t)),
    '=': (f, l, r) => assign(f, l, f(r)),
    if: (f, a, ...b) => f.put('__if', f(a)) && statement(f, b),
    else: (f, ...a) => f.get('__if') || statement(f, a),
    iif: (f, ...a) => iif(f, a),
    case: (f, ...a) => case_(f, f(a[0]), a.slice(1)),
    catch: (f, a, b) => attempt(() => f(a), e => f.call(f(b), new Record(e))),
    while: (f, cond, ...body) => next(f, cond, body, null)
  }
  const lazy = f => (f.lazy = true, f)
  const embedded = {
    none,
    some: x => new Some(x),
    continue: new Continue(),
    break: new Break(),
    assert: (l, r) => compare(l, r, (a, b) => a === b || fail('assert', {l, r, a, b})),
    throw: x => { throw new Error(x) },
    return: x => new Return(x),
    regexp: s => new RegExp(s, 'g'),
    tuple: (...a) => new Tuple().concat(a),
    list: (...a) => a,
    dict: (...a) => new Map(a.length ? [...Array(a.length/2)].map((_,i) => [a[i*2], a[i*2+1]]) : []),
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
    ...Object.fromEntries(Object.keys(laziness).map(k => [k, lazy(laziness[k])])),
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
