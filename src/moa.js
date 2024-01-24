'use strict'
const util = require('util')
const show = o => Array.isArray(o) ? '(list' + o.map(show).map(x => ' ' + x).join('') + ')' :
  o instanceof Map ? '(dict' + [...o].flatMap(a => a.map(show)).map(x => ' ' + x).join('') + ')' :
  o.toString()
const print = (...a) => (console.log(...a.map(show)), a[0])
const log = (...a) => (console.error(...a.map(o => util.inspect(o, false, null, true))), a[0])
const attempt = f => { try { return f() } catch (e) { return e } }
const loop = (f, g) => { const a = []; while (f()) { a.push(g()) }; return a }
const fail = (m, o) => { log(o); throw new Error(m) }

const execute = (source, embedded) => {
  // parser
  let offset = 0
  let lineid = 1
  const tokens = source.split(/([(){};.]|"[^]*?(?<!\\)"|(?:#[^\n]*|\s+))/).flatMap(code => {
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
    target.code === 'true' ? true :
    target.code === 'false' ? false :
    target.code.match(/^[0-9]/) ? parseFloat(target.code) :
    target.code.match(/^["'`]/) ? target.code.slice(1, -1) :
    target.code in env ? env[target.code] :
    fail('Missing', {target, ids: [...Object.keys(env)]})
  const props = {
    'String size': o => o.length,
    'Array size': o => o.length,
  }
  const prop = (obj, name) => {
    const p = props[`${obj.constructor.name} ${name}`]
    return p ? p(obj) : typeof obj === 'object' && name in obj ? obj[name] : fail('NoProperty', {obj, name})
  }
  const map = (a, b) => Object.fromEntries(a.map((x,i) => [x,b[i]]))
  Object.assign(embedded, {
    def: (env, [name, a, body]) => env[name.code] = (e, b) =>
      run({...e, ...map(a.map(x => x.code), b.map(exp => run(e, exp)))}, body),
    var: (env, [name, exp]) => env[name.code] = run(env, exp),
    let: (env, [name, exp]) => env[name.code] = run(env, exp),
    struct: (env, [name, fields]) => env[name.code] = (e, a) =>
      map(fields.map(f => f[0].code), a.map(exp => run(e, exp))),
    log: (env, a) => log(...a.map(exp => run(env, exp))),
    print: (env, a) => print(...a.map(exp => run(env, exp))),
    list: (env, a) => a.map(exp => run(env, exp)),
    dict: (env, a) => (a => new Map([...new Array(a.length/2)].map((_, i) => [a[i*2], a[i*2+1]])))(a.map(x => run(env, x))),
    '.': (env, [obj, name]) => prop(run(env, obj), name.code),
    '{': (env, lines) => lines.map(line => run(env, line)).at(-1),
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
    ['++', (l, r) => l instanceof Map ? new Map([...l, ...r]) : l.concat(r)],
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
