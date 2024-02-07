'use strict'

process.env.TZ = 'UTC'

class Tuple extends Array {}
class None {}
class Some   { constructor(value) { this.__value = value } }
class Ref    { constructor(value) { this.__value = value } }
class Return { constructor(value) { this.__value = value } }
class Continue {}
class Break {}
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
const unwrap = (o, t) => o instanceof t ? o.__value : o

const fs = require('node:fs')
const util = require('node:util')
const { execSync } = require('child_process')
const log = (...a) => (console.error(...a.map(o => util.inspect(o, false, null, true))), a[0])
const attempt = (f, g) => { try { return f() } catch (e) { return g ? g(e) : e } }
const fail = (m, o) => { const e = new Error(m); e.detail = o; throw e }
const tuple = (...a) => new Tuple().concat(a)
const comp = (a, b, f) =>
  a === undefined ? b === undefined :
  a === null ? b === null :
  typeof a === 'string'  && typeof b === 'string'  ? f(a, b) :
  typeof a === 'number'  && typeof b === 'number'  ? f(a, b) :
  typeof a === 'boolean' && typeof b === 'boolean' ? f(a, b) :
  a.constructor.name !== b.constructor.name ? fail(`${a.constructor.name} and ${b.constructor.name} are can not be compared`) :
  a instanceof Error ? a.message === b.message :
  a instanceof Array ? a.length === b.length && a.every((x,i) => comp(x, b[i], f)) :
  a instanceof Map ? [...a.keys()].sort().map(k => comp(a.get(k), b.get(k), f)) :
  typeof object ? Object.keys(a).sort().every(k => comp(a[k], b[k], f)) :
  fail(`${a} and ${b} are can not be compared`)
const stdin = { utf8: fs.readFileSync('/dev/stdin', 'utf8') } // TODO: to be passed via embedded argument

const parse = source => {
  // parser
  let offset = 0
  let lineid = 1
  let indent = 0
  // operator | symbols | id | number | string | white space
  const tokens = source.split(/([+\-*\/%|&<>!=]+|[(){};.]|[A-Za-z_][0-9A-Za-z_]*|[0-9]+(?:\.[0-9]+)?|".*?(?<!\\)"|(?:#[^\n]*|\s+))/s).flatMap(code => {
    offset += code.length
    lineid += (code[0] !== '"' ? code.split('\n').length - 1 : 0) + (code === ';' ? 1 : 0)
    indent = code[0] !== '"' && code.includes('\n') ? code.split('\n').at(-1).length : indent
    const enabled = code !== ';' && !/^\s*#/.test(code) && code.trim()
    return enabled ? [{code, lineid, offset, indent}] : []
  })
  let pos = 0
  const many = (f, acc) => pos >= tokens.length ? acc || [] :
    ((x, acc) => x ? many(f, acc.concat([x])) : acc)(f(tokens[pos]), acc || [])
  const unlist = a => a.length === 1 ? a[0] : a
  const until = code => many(t => t.code === code ? (++pos, null) : consume())
  const op2s = '|| && = == != < <= > >= ++ + - | & ^ ~ * / % ^ **'.split(' ') // low...high, other operators are lowest
  const ops = ['!', '=>', ...op2s]
  const priority = t => op2s.findIndex(op => op === t.code)
  const isOp = t => t && t.code && ops.includes(t.code)
  const isOp2 = t => t && t.code && (op2s.includes(t.code) || op2s.find(op => t.code === op + '='))
  const reorder = (op, l, r) => isOp2(r[0]) && priority(r[0]) < priority(op) ?
    [r[0], [op, l, r[1]], r[2]] :
    [op, l, r]
  const suffix = x => {
    if (pos >= tokens.length) {
      return x
    }
    const t = tokens[pos++]
    const near = t.offset - t.code.length === tokens[pos-2].offset
    return near && t.code === '(' ? suffix([x, ...until(')')]) :
      near && t.code === '[' ? suffix([t, x, unlist(until(']'))]) :
      near && t.code === '.' ? suffix([t, x, tokens[pos++]]) :
      t.code === '=>' ? arrow(x, t, tokens[pos].indent) :
      isOp2(t) ? reorder(t, x, consume()) :
      (--pos, x)
  }
  const parenthesis = a => isOp(a[0]) ? a : a.length === 1 ? (isOp(a[0][0]) ? a[0] : a) : fail('TooManyInParenthesis', a)
  const arrow = (x, t, indent) =>
    tokens[pos].lineid === t.lineid ? [t, x, consume()] :
    [t, x, [{code: ':'}, ...many(t => t.indent === indent && line(t))]]
  const block = (t, indent) =>
    tokens[pos].lineid === t.lineid ? line(t) :
    [t, ...many(t => t.indent === indent && line(t))]
  const consume = () => (t =>
    t.code === '!' ? [t, suffix(consume())] :
    t.code === '(' ? suffix(parenthesis(until(')'))) :
    t.code === ':' ? block(t, tokens[pos].indent) :
    suffix(t))(tokens[pos++])
  const line = t => unlist(many(u => u.lineid === t.lineid && u.code !== ')' && consume()))
  return many(line)
}

const execute = (source, embedded) => {
  const nodes = parse(source)
  const none = new None()
  const qmap = {'r': '\r', 'n': '\n', 't': '\t'}
  const unquote = s => s.replace(/\\(.)/g, (_, c) => qmap[c] || c)
  const call = (env, f, a) => typeof f === 'function' ? f(env, ...a) : fail('NotFunction', {f, a})
  const run = (env, target) =>
    Array.isArray(target) ? call(env, run(env, target[0]), target.slice(1)) :
    'raw' in target ? target.raw :
    target.code === 'true' ? true :
    target.code === 'false' ? false :
    target.code.match(/^[0-9]/) ? parseFloat(target.code) :
    target.code.match(/^["'`]/) ? unquote(target.code.slice(1, -1)) :
    target.code in env ? unwrap(env[target.code], Ref) :
    fail(`can not find value \`${target.code}\` in this scope`, {target, ids: [...Object.keys(env)]})
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
       .replace('Z', t.offset === 0 ? 'Z' : t.offset)
       .replace('z', zone(t.offset))
  const rg = r => new RegExp(r, r.flags.replace('g', '') + 'g')
  const lambda = f => (env, ...a) => f(...a.map(x => run(env, x)))
  const func = f => (env, ...a) => f(env, ...a.map(x => run(env, x)))
  const at = (a, i) => 0 <= i && i < a.length ? a[i] :
    i < 0 && Math.abs(i) <= a.length ? a[a.length + i] :
    fail('OutOfIndex', {a, i})
  const tie = (a, i, v) => 0 <= i && i < a.length ? a[i] = v :
    i < 0 && Math.abs(i) <= a.length ? a[a.length + i] = v :
    fail('OutOfIndex', {a, i, v})
  const props = {
    'String size': s => s.length,
    'String slice': s => lambda((...a) => s.slice(...a)),
    'String split': s => lambda((...a) => s.split(...a)),
    'String reverse': s => s.split('').reverse().join(''),
    'String replace': s => lambda((...a) => s.replaceAll(...a)),
    'String index': s => lambda(t => (n => n === -1 ? none : new Some(n))(s.indexOf(t))),
    'String starts': s => lambda(t => s.startsWith(t)),
    'String ends': s => lambda(t => s.endsWith(t)),
    'String trim': s => s.trim(),
    'String has': s => lambda(t => s.includes(t)),
    'RegExp match': r => lambda(s => r.test(s)),
    'RegExp capture': r => lambda(s => (a => a === null ? [] : a)(s.match(rg(r)))),
    'RegExp split': r => lambda(s => s.split(r)),
    'RegExp replace': r => func((env, s, f) => s.replace(rg(r), (...a) => f(env, ...a.slice(0, -2).map(raw)))),
    'Array at': a => lambda(i => at(a, i)),
    'Array tie': a => lambda((i, v) => tie(a, i, v)),
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
  const iif = (env, a) => _iif(env,
    a.length === 1 && a[0][0].code === ':' ? a[0].slice(1).flatMap(x => x) :
    a.length === 2 && a[1][0].code === ':' ? [a[0], ...a[1].slice(1)] :
    a)
  const _iif = (env, a) => a.length === 0 ? fail('NotEnoughIif') :
    a.length === 1 ? run(env, a[0]) :
    run(env, a[0]) ? run(env, a[1]) :
    _iif(env, a.slice(2))
  const case_ = (env, target, a) => a.length <= 1 ? fail('NotEnoughCase', target) :
    a[0].code === '_' || comp(target, run(env, a[0]), (x,y) => x === y) ? run(env, a[1]) :
    case_(env, target, a.slice(2))
  const while_ = (env, cond, a) => {
    while (run(env, cond)) {
      const ret = statement(env, a)
      if (ret instanceof Return) {
        return ret
      }
      if (ret instanceof Break) {
        break
      }
    }
    return none
  }
  const map = (a, b) => Object.fromEntries(a.map((x,i) => [x,b[i]]))
  const statement = (env, a) => (env => a.reduce((acc, x) =>
    acc instanceof Return || acc instanceof Continue || acc instanceof Break ? acc : run(env, x), null))({...env})
  const fn = (env, ...a) => (e, ...b) =>
      unwrap(run({...e, ...map(a.slice(0, -1).map(x => x.code), b.map(exp => run(e, exp)))}, a.at(-1)), Return)
  const declar = (env, a, b) => Array.isArray(a) && a[0].code === '[' ? tie(run(env, a[1]), run(env, a[2]), b) :
    env[a.code] = b
  const update = (env, a, b) => Array.isArray(a) && a[0].code === '[' ? tie(run(env, a[1]), run(env, a[2]), b) :
    env[a.code] instanceof Ref ? env[a.code].__value = b :
    fail(`can not assign twice to immutable variable \`${a.code}\``, a)
  const fields = body => body[0].code === ':' ? body.slice(1) : [body]
  const unblock = a => a.length === 1 && a[0][0].code === ':' ? a[0].slice(1).flatMap(x => x) : a
  Object.assign(embedded, {
    fn: fn,
    '=>': fn,
    def: (env, name, ...a) => declar(env, name, fn(env, ...a)),
    var: (env, name, exp) => declar(env, name, new Ref(run(env, exp))),
    let: (env, name, exp) => declar(env, name, run(env, exp)),
    record: (env, name, body) => declar(env, name, (e, ...a) => map(fields(body).map(f => f[0].code), a.map(exp => run(e, exp)))),
    struct: (env, ...a) => Object.fromEntries(a.map(x => Array.isArray(x) ? [x[1].code, run(env, x[2])] : [x.code, run(env, x)])),
    list: lambda((...a) => a),
    set: lambda((...a) => new Set(a)),
    dict: lambda((...a) => new Map(a.length ? [...new Array(a.length/2)].map((_, i) => [a[i*2], a[i*2+1]]) : [])),
    regexp: lambda((s, o) => new RegExp(s, o)),
    tuple: lambda(tuple),
    time: lambda((...a) => new Time(...a)),
    some: lambda(x => new Some(x)),
    none: none,
    log: lambda((...a) => (console.error(...a), a[0])),
    continue: new Continue(),
    break: new Break(),
    assert: (env, a, b) => {
      const x = run(env, a)
      const y = b === undefined ? true : run(env, b)
      comp(x, y, (x, y) => x === y) || fail('Assert', {a, b, x, y})
    },
    if: (env, cond, body) => (env.__if = run(env, cond)) && run({...env}, body),
    else: (env, body) => !env.__if && (delete env.__if, run(env, body)),
    iif: (env, ...a) => iif(env, a),
    case: (env, a, ...b) => case_(env, run(env, a), unblock(b)),
    while: (env, cond, ...a) => while_(env, cond, a),
    return: lambda(x => new Return(x)),
    throw: lambda(fail),
    catch: (env, x, f) => attempt(() => run(env, x), e => run(env, f)(env, raw(e))),
    '.': (env, obj, name) => prop(run(env, obj), name.code),
    '[': lambda((a, i) => at(a, i)),
    '=': (env, a, b) => update(env, a, run(env, b)),
    do: (env, ...a) => statement(env, a),
    ':': (env, ...a) => statement(env, a),
    io: {
      get rand() { return Math.random() },
      argv: process.argv.slice(2),
      print: lambda((...a) => console.log(...a)),
      shell: (env, ...a) => {
        const cmd = a.map(x => run(env, x)).join(' ').replace(/'/g, "'")
        const result = execSync(cmd, {encoding: 'utf8', stdio: ['ignore', 'pipe', 'ignore']})
        return {result}
      },
      stdin,
    },
  })
  const defineOp2 = (op, opf) => {
    embedded[op] = (env, head, ...a) => a.reduce((acc, x) => opf(acc, run(env, x)) , run(env, head)),
    embedded[op + '='] = (env, l, r) => update(env, l, opf(run(env, l), run(env, r)))
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
  embedded['-='] = (env, a) => { log(a); const l=a[0]; const r=a[1]; return update(env, l.code, run(env, l) - run(env, r)) }
  embedded['!'] = lambda(x => !x)
  embedded['=='] = lambda((l,r) => comp(l, r, (x,y) => x === y))
  embedded['!='] = lambda((l,r) => comp(l, r, (x,y) => x !== y))
  embedded['>']  = lambda((l,r) => comp(l, r, (x,y) => x >   y))
  embedded['>='] = lambda((l,r) => comp(l, r, (x,y) => x >=  y))
  embedded['<']  = lambda((l,r) => comp(l, r, (x,y) => x <   y))
  embedded['<='] = lambda((l,r) => comp(l, r, (x,y) => x <=  y))
  return nodes.map(node => run(embedded, node)).at(-1)
}

const compile = source => {
  const nodes = parse(source)
  const def = (name, args, body) => `func ${name.code}(${args.map(x => `${x.code} interface{}`).join(', ')}) { ${gen(body)} }`
  const gen = a =>
    !Array.isArray(a) ? a.code :
    a[0].code === 'def' ? def(a[1], a.slice(2, -1), a.at(-1)) :
    a[0].code === '.' ? `${gen(a[1])}.${a[2].code}` :
    `${gen(a[0])}(${a.slice(1).map(gen).join(', ')})`
  return fs.readFileSync(__dirname + '/base.go', 'utf8') + nodes.map(gen).join('\n')
}

if (process.argv[2] === 'go') {
  console.log(compile(stdin.utf8))
} else if (process.argv[2] === 'parse') {
  log(parse(stdin.utf8))
} else {
  const env = {}
  for (const chunk of stdin.utf8.split(/\n(?=assert )/mg)) {
    try {
      execute(chunk, env)
    } catch (e) {
      console.log(`echo '${chunk.trim()}' | node src/moa.js`)
      console.dir(e, {depth: null})
      process.exit(1)
    }
  }
  env.main && env.main(env)
}
