'use strict'
class Tuple extends Array {}
const util = require('node:util')
const show = o =>
  o instanceof Tuple ? '(tuple' + o.map(show).map(x => ' ' + x).join('') + ')' :
  o instanceof Array ? '(list' + o.map(show).map(x => ' ' + x).join('') + ')' :
  o instanceof Map ? '(dict' + [...o].flatMap(a => a.map(show)).map(x => ' ' + x).join('') + ')' :
  o instanceof Set ? '(set' + [...o].map(show).map(x => ' ' + x).join('') + ')' :
  o instanceof RegExp ? `(regexp ${o.toString().slice(1, -1)})` :
  o.toString()
const log = (...a) => (console.error(...a.map(o => util.inspect(o, false, null, true))), a[0])
const attempt = f => { try { return f() } catch (e) { return e } }
const loop = (f, g) => { const a = []; while (f()) { a.push(g()) }; return a }
const fail = (m, o) => { log(o); throw new Error(m) }
const tuple = (...a) => new Tuple().concat(a)

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
  const call = (env, f, a) => typeof f === 'function' ? f(env, a) : fail('NotFunction', {f, a})
  const run = (env, target) =>
    Array.isArray(target) ? call(env, run(env, target[0]), target.slice(1)) :
    'raw' in target ? target.raw :
    target.code === 'true' ? true :
    target.code === 'false' ? false :
    target.code.match(/^[0-9]/) ? parseFloat(target.code) :
    target.code.match(/^["'`]/) ? target.code.slice(1, -1) :
    target.code in env ? env[target.code] :
    fail('Missing', {target, ids: [...Object.keys(env)]})
  const lambda = f => (env, a) => f(...a.map(x => run(env, x)))
  const raw = o => ({raw: o})
  const props = {
    'String size': s => s.length,
    'String slice': s => lambda((...a) => s.slice(...a)),
    'String split': s => lambda((...a) => s.split(...a)),
    'String reverse': s => s.split('').reverse().join(''),
    'String replace': s => lambda((...a) => s.replaceAll(...a)),
    'RegExp test': r => lambda(s => r.test(s)),
    'RegExp split': r => lambda(s => s.split(r)),
    'RegExp replace': r => (env, [s, f]) => (f => run(env, s).replace(new RegExp(r, r.flags.replace('g', '') + 'g'), (...a) => f(env, a.slice(0, -2).map(raw))))(run(env, f)),
    'Array size': a => a.length,
    'Array slice': a => lambda((...b) => a.slice(...b)),
    'Array reverse': a => a.reverse(),
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
  }
  const prop = (obj, name) => {
    const p = props[`${obj.constructor.name} ${name}`]
    return p ? p(obj) : typeof obj === 'object' && name in obj ? obj[name] : fail('NoProperty', {obj, name})
  }
  const map = (a, b) => Object.fromEntries(a.map((x,i) => [x,b[i]]))
  Object.assign(embedded, {
    fn: (env, [a, body]) => (e, b) =>
      run({...e, ...map(a.map(x => x.code), b.map(exp => run(e, exp)))}, body),
    def: (env, [name, a, body]) => env[name.code] = (e, b) =>
      run({...e, ...map(a.map(x => x.code), b.map(exp => run(e, exp)))}, body),
    var: (env, [name, exp]) => env[name.code] = run(env, exp),
    let: (env, [name, exp]) => env[name.code] = run(env, exp),
    struct: (env, [name, fields]) => env[name.code] = (e, a) =>
      map(fields.map(f => f[0].code), a.map(exp => run(e, exp))),
    log: lambda((...a) => (console.error(...a.map(show)), a[0])),
    list: lambda((...a) => a),
    set: lambda((...a) => new Set(a)),
    dict: lambda((...a) => new Map([...new Array(a.length/2)].map((_, i) => [a[i*2], a[i*2+1]]))),
    regexp: lambda(s => new RegExp(s)),
    tuple: lambda((...a) => tuple(...a)),
    '.': (env, [obj, name]) => prop(run(env, obj), name.code),
    '{': (env, lines) => lines.map(line => run(env, line)).at(-1),
  })
  const defineOp2 = (op, opf) => {
    embedded[op] = (env, [head, ...a]) => a.reduce((acc, x) => opf(acc, run(env, x)) , run(env, head)),
    embedded[op + '='] = (env, [l, r]) => env[l.code] = opf(run(env, l), run(env, r))
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
