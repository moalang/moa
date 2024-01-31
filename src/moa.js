'use strict'
process.env.TZ = 'UTC'
class Tuple extends Array {}
class None {}
class Some {
  constructor(value) {
    this.__value = value
  }
}
class Time {
  constructor(year, month, day, hour, min, sec, offset) {
    const d = new Date(year, month - 1, day, hour, min, sec)
    this.year = d.getFullYear()
    this.month = d.getMonth() + 1
    this.day = d.getDate()
    this.hour = d.getHours()
    this.min = d.getMinutes()
    this.sec = d.getSeconds()
    this.offset = offset || 0
    this.wday = d.getDay()
    const s = new Date(Date.UTC(year, 0, 1))
    const e = new Date(Date.UTC(year, month - 1, day))
    this.yday = Math.floor((e - s) / (1000 * 60 * 60 * 24)) + 1
  }
}
const none = new None()
const util = require('node:util')
const log = (...a) => (console.error(...a.map(o => util.inspect(o, false, null, true))), a[0])
const attempt = (f, g) => { try { return f() } catch (e) { return g ? g(e) : e } }
const loop = (f, g) => { const a = []; while (f()) { a.push(g()) }; return a }
const fail = (m, o) => { o && log(o); throw new Error(m) }
const tuple = (...a) => new Tuple().concat(a)
const comparable = x =>
  x === undefined ? 'undefined' :
  x === null ? 'null' :
  x.constructor.name + ':' + (
    x instanceof Error ? e.message :
    Array.isArray(x) ? x.map(comparable).join(' ') :
    x instanceof Map ? [...x.keys()].sort().map(key => comparable(key + ':' + x.get(key))).join(' ') :
    typeof x === 'object' ? Object.keys(x).sort().map(key => key + ':' + comparable(x[key])).join(' ') :
    typeof x === 'number' ? (Array(16).join('0') + x).slice(-16) :
    typeof x === 'string' ? x :
    typeof x === 'boolean' ? x.toString() :
    fail('UnComparable', x))

const execute = (source, embedded) => {
  // parser
  let offset = 0
  let lineid = 1
  // operator | symbols | number | string | white space
  const tokens = source.split(/([+\-*\/%|&<>!=]+|[(){};.]|[0-9]+(?:\.[0-9]+)?|"[^]*?(?<!\\)"|(?:#[^\n]*|\s+))/).flatMap(code => {
    offset += code.length
    lineid += code.split('\n').length - 1 + (code === ';' ? 1 : 0)
    const enabled = code !== ';' && !/^\s*#/.test(code) && code.trim()
    return enabled ? [{code, lineid, offset}] : []
  })
  let pos = 0
  const many = (f, g) => loop(() => pos < tokens.length && (!g || g(tokens[pos])), () => f(tokens[pos++]))
  const until = (f, g) => (a => (++pos, a))(many(f, g))
  const op2s = '|| && == != < <= > >= + - * / % ^ **'.split(' ') // low...high, other operators are lowest
  const priority = t => op2s.findIndex(op => op === t.code)
  const isOp2 = t => t && /^[+\-*\/%<>!=~.]/.test(t.code)
  const reorder = xs => {
    if (xs.length === 1) {
      return xs
    }
    const stack = []
    for (let i=0; i<xs.length; i++) {
      const x = xs[i]
      if (x.code === '!') {
        stack.push([x, xs[++i]])
      } else if (isOp2(x) && stack.length) {
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
    return xs.length <= 3 && stack.length === 1 ? stack[0] : stack
  }
  const list = () => reorder(until(unit, t => t.code !== ')'))
  const unit = t => t.code === '(' ? list() :
    t.code === '{' ? [t].concat(until(top, t => t.code !== '}')) :
    t
  const line = l => {
    const a = many(unit, t => t.lineid === l && t.code !== '}')
    return a.length === 1 ? a[0] : reorder(a)
  }
  const top = t => t.code === '(' ? list() : (--pos, line(t.lineid))
  const nodes = many(top)

  // interpriter
  const qmap = {'n': '\n', 't': '\t', '\\': '\\'}
  const unquote = s => s.replace(/\\(.)/g, (_, c) => qmap[c] || c)
  const call = (env, f, a) => typeof f === 'function' ? f(env, ...a) : fail('NotFunction', {f, a})
  const run = (env, target) =>
    Array.isArray(target) ? call(env, run(env, target[0]), target.slice(1)) :
    'raw' in target ? target.raw :
    target.code === 'true' ? true :
    target.code === 'false' ? false :
    target.code.match(/^[0-9]/) ? parseFloat(target.code) :
    target.code.match(/^["'`]/) ? unquote(target.code.slice(1, -1)) :
    target.code in env ? env[target.code] :
    fail('Missing', {target, ids: [...Object.keys(env)]})
  const raw = o => ({raw: o})
  const z2 = s => ('0' + s).slice(-2)
  const z4 = s => ('0000' + s).slice(-4)
  const zone = n => {
    const sign = n >= 0 ? '+' : '-'
    const h = Math.floor(Math.abs(n) / 60 / 60)
    const m = Math.floor((Math.abs(n) - (h * 60 * 60)) / 60)
    const s = Math.floor(Math.abs(n) - h * 60 * 60 - m * 60)
    return sign + z2(h) + m + s
  }
  const format = (t, s) =>
      s.replace('yyyy', z4(t.year))
       .replace('mm', z2(t.month))
       .replace('dd', z2(t.day))
       .replace('HH', z2(t.hour))
       .replace('MM', z2(t.min))
       .replace('SS', z2(t.sec))
       .replace('m', t.month)
       .replace('d', t.day)
       .replace('H', t.hour)
       .replace('M', t.min)
       .replace('S', t.sec)
       .replace('Z', t.offset === 0 ? 'Z' :t.offset)
       .replace('z', zone(t.offset))
  const rg = r => new RegExp(r, r.flags.replace('g', '') + 'g')
  const lambda = f => (env, ...a) => f(...a.map(x => run(env, x)))
  const func = f => (env, ...a) => f(env, ...a.map(x => run(env, x)))
  const props = {
    'String size': s => s.length,
    'String slice': s => lambda((...a) => s.slice(...a)),
    'String split': s => lambda((...a) => s.split(...a)),
    'String reverse': s => s.split('').reverse().join(''),
    'String replace': s => lambda((...a) => s.replaceAll(...a)),
    'String index': s => lambda(t => (n => n === -1 ? none : new Some(n))(s.indexOf(t))),
    'RegExp match': r => lambda(s => (a => a === null ? none : new Some(a))(s.match(rg(r)))),
    'RegExp split': r => lambda(s => s.split(r)),
    'RegExp replace': r => func((env, s, f) => s.replace(rg(r), (...a) => f(env, ...a.slice(0, -2).map(raw)))),
    'Array size': a => a.length,
    'Array slice': a => lambda((...b) => a.slice(...b)),
    'Array reverse': a => a.reverse(),
    'Array get': a => lambda(i => 0 <= i && i < a.length ? new Some(a[i]) : none),
    'Array set': a => lambda((i, v) => 0 <= i && i < a.length ? (a[i] = v, true) : false),
    'Array map': a => func((env, f) => a.map(x => f(env, raw(x)))),
    'Array fmap': a => func((env, f) => a.flatMap(x => f(env, raw(x)))),
    'Array keep': a => func((env, f) => a.filter(x => f(env, raw(x)))),
    'Array all': a => func((env, f) => a.every(x => f(env, raw(x)))),
    'Array any': a => func((env, f) => a.some(x => f(env, raw(x)))),
    'Array sort': a => func((env, f) => f ? a.toSorted((x, y) => f(env, raw(x), raw(y))) : a.toSorted()),
    'Array zip': a => lambda(b => a.map((x, i) => tuple(x, b[i]))),
    'Array fold': a => func((env, v, f) => a.reduce((acc, x) => f(env, raw(acc), raw(x)), v)),
    'Array find': a => func((env, f) => (r => r === undefined ? none : new Some(r))(a.find(x => f(env, raw(x))))),
    'Array join': a => lambda(s => a.join(s)),
    'Array has': a => lambda(x => a.includes(x)),
    'Array min': a => Math.min(...a),
    'Array max': a => Math.max(...a),
    'Map get': m => lambda(k => m.has(k) ? new Some(m.get(k)) : none),
    'Map set': m => lambda((k, v) => (b => (m[k] = v, !b))(m.has(k))),
    'Map has': m => lambda(k => m.has(k)),
    'Map keys': m => [...m.keys()],
    'Map values': m => [...m.values()],
    'Map list': m => [...m].map(([k,v]) => tuple(k, v)),
    'Set has': s => lambda(x => s.has(x)),
    'Set add': s => lambda(x => s.has(x) ? false : (s.add(x), true)),
    'Set rid': s => lambda(x => s.delete(x)),
    'Set list': s => [...s],
    'Number abs': n => Math.abs(n),
    'Number neg': n => -n,
    'Number char': n => String.fromCharCode(n),
    'Number floor': n => Math.floor(n),
    'Number ceil': n => Math.ceil(n),
    'Number round': n => Math.round(n),
    'Time utc': t => new Time(t.year, t.month, t.day, t.hour, t.min, t.sec - t.offset, 0),
    'Time string': t => format(t, 'yyyy-mm-ddTHH:MM:SSZ'),
    'Time format': t => lambda(s => format(t, s)),
    'Time tick': t => lambda(n => new Time(t.year, t.month, t.day, t.hour, t.min, t.sec + n, t.offset)),
    'Some and': s => (env, f) => new Some((run(env, f))(env, raw(s.__value))),
    'Some or': s => () => s.__value,
    'None and': s => () => none,
    'None or': s => lambda(x => x),
  }
  const prop = (obj, name) => {
    const p = props[`${obj.constructor.name} ${name}`]
    return p ? p(obj) : typeof obj === 'object' && name in obj ? obj[name] : fail('NoProperty', {obj, name})
  }
  const map = (a, b) => Object.fromEntries(a.map((x,i) => [x,b[i]]))
  Object.assign(embedded, {
    fn: (env, a, body) => (e, ...b) =>
      run({...e, ...map(a.map(x => x.code), b.map(exp => run(e, exp)))}, body),
    def: (env, name, a, body) => env[name.code] = (e, ...b) =>
      run({...e, ...map(a.map(x => x.code), b.map(exp => run(e, exp)))}, body),
    var: (env, name, exp) => env[name.code] = run(env, exp),
    let: (env, name, exp) => env[name.code] = run(env, exp),
    struct: (env, name, fields) => env[name.code] = (e, ...a) =>
      map(fields.map(f => f[0].code), a.map(exp => run(e, exp))),
    log: lambda((...a) => (console.error(...a), a[0])),
    list: lambda((...a) => a),
    set: lambda((...a) => new Set(a)),
    dict: lambda((...a) => new Map(a.length ? [...new Array(a.length/2)].map((_, i) => [a[i*2], a[i*2+1]]) : [])),
    regexp: lambda(s => new RegExp(s)),
    tuple: lambda((...a) => tuple(...a)),
    time: lambda((...a) => new Time(...a)),
    some: lambda(x => new Some(x)),
    none: none,
    assert: (env, a, b) => {
      const show = o => Array.isArray(o) ? `(${o.map(show).join(' ')})` : o.code.toString()
      const code = `(assert ${show(a)} ${show(b)})`
      try {
        const x = comparable(run(env, a))
        const y = comparable(run(env, b))
        x === y || fail('Assert', {code, x, y})
      } catch (e) {
        fail('Assert', {code, e})
      }
    },
    throw: lambda((...a) => fail(...a)),
    catch: (env, x, f) => attempt(() => run(env, x), e => run(env, f)(env, raw(e))),
    '.': (env, obj, name) => prop(run(env, obj), name.code),
    '{': (env, lines) => lines.map(line => run(env, line)).at(-1),
  })
  const defineOp2 = (op, opf) => {
    embedded[op] = (env, head, ...a) => a.reduce((acc, x) => opf(acc, run(env, x)) , run(env, head)),
    embedded[op + '='] = (env, l, r) => env[l.code] = opf(run(env, l), run(env, r))
  }
  defineOp2('+', (l, r) => l + r)
  defineOp2('*', (l, r) => l * r)
  defineOp2('/', (l, r) => l / r)
  defineOp2('%', (l, r) => l % r)
  defineOp2('|', (l, r) => l instanceof Set ? new Set([...l, ...r]) : l | r)
  defineOp2('&', (l, r) => l instanceof Set ? new Set([...l].filter(x => r.has(x))) : l & r)
  defineOp2('^', (l, r) => l instanceof Set ? new Set([...l, ...r].flatMap(x => l.has(x) && r.has(x) ? [] : [x])) : l ^ r)
  defineOp2('||', (l, r) => l || r)
  defineOp2('&&', (l, r) => l && r)
  defineOp2('**', (l, r) => l ** r)
  defineOp2('++', (l, r) => l instanceof Map ? new Map([...l, ...r]) : l.concat(r))
  const minus = (l, r) => l instanceof Set ? new Set([...l].filter(x => !r.has(x))) : l - r
  embedded['-'] = lambda((...a) => a.length === 1 ? -a[0] : a.reduce((acc,n) => acc === undefined ? n : minus(acc, n)))
  embedded['-='] = (env, [l, r]) => env[l.code] = run(env, l) - run(env, r)
  embedded['!'] = lambda(x => !x)
  embedded['=='] = lambda((l,r) => comparable(l) === comparable(r))
  embedded['!='] = lambda((l,r) => comparable(l) !== comparable(r))
  embedded['>']  = lambda((l,r) => comparable(l) >   comparable(r))
  embedded['>='] = lambda((l,r) => comparable(l) >=  comparable(r))
  embedded['<']  = lambda((l,r) => comparable(l) <   comparable(r))
  embedded['<='] = lambda((l,r) => comparable(l) <=  comparable(r))
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

if (process.stdin.isRaw === undefined) {
  const fs = require('node:fs')
  execute(fs.readFileSync('/dev/stdin', 'utf8'), {})
} else {
  repl()
}
