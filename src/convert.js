/*
 * This program converts some nodes in an internal expression based on type inference.
 * [x] Basic type inference
 * [ ] Error handling
 * [ ] Convert a method call to a function call
 */
const dump = o => { console.dir(o, {depth: null}); return o }
const fail = m => { throw new Error(m) }
const failInfer = (m,l,r) => { const e = new Error(m); e.l = l; e.r = r; throw e }
const str = o => typeof o === 'string' ? o :
  Array.isArray(o) ? `(${o.map(str).join(' ')})${o.repeatable ? '*' : ''}` :
  o instanceof String ? o.toString() :
  typeof o === 'object' && o.variable ? (s => (o.variable=true, `var(${s})`))((o.variable=false, str(o))) :
  typeof o === 'object' && o.instance ? str(o.instance) :
  typeof o === 'object' && (o.tid || o.type || o.name) ? o.toString() + (o.repeatable ? '*' : '') + (o.errors ? o.errors.map(e => `|${str(e)}`).join('') : '') :
  JSON.stringify(o)
const put = (...a) => { process.stdout.write(a.map(str).join(' ')); return a[0] }
const puts = (...a) => { console.log(a.map(str).join(' ')); return a[0] }
const infer = root => {
  let unique = 1
  const assertVariable = t => t.variable ? t : failInfer(`'${t}' is not variable`, `var(${t.toString()})`, t)
  const variable = t => (t.variable = true, t)
  const repeat = t => (t.repeatable = true, t)
  const errable = (t, error) => ((t.errors = t.errors || []).push(error), t)
  const referable = (t, s) => (t.toString = () => t.instance ? t.instance.toString() : s, t)
  const tvar = () => (tid => referable({tid, instance: null},  tid.toString()))(unique++)
  const tclass = name => referable({name}, name)
  const type = (name, ...generics) => ({name, generics, errors: [], toString: () => `${name}${generics.length ? `(${generics.map((g, i) => g.instance ? g.instance.toString() : g.toString()).join(' ')})` : ''}`})
  const prune = t => t.instance ? t.instance = prune(t.instance) : t
  const tbool = type('bool')
  const tint = type('int')
  const tfloat = type('float')
  const tstring = type('string')
  const tregexp = type('regexp')
  const tclasses = {
    num: [tint, tfloat],
  }
  const tenv = {
    int: tint,
    'true': tbool,
    'false': tbool,
    'string': () => [tvar(), tstring],
    'int': [type('num'), tint],
    'float': [type('num'), tfloat],
    'list': () => (t => [t, type('list', t)])(repeat(tvar())),
    'set': () => (t => [t, type('set', t)])(repeat(tvar())),
    'dict': () => (a => [a, type('dict', a[0], a[1])])(repeat([tvar(), tvar()])),
    'throw': () => (t => [t, errable(tvar(), t)])(tvar())
  }
  const tput = (k, v) => tenv[k] = v
  '+ - * / % ** // += -= /= *= /= %= **='.split(' ').map(op => tput(op, (t => [t, t, t])(type('num'))))
  '&& ||'.split(' ').map(op => tput(op, [tbool, tbool, tbool]))
  '== != < <= > >='.split(' ').map(op => tput(op, () => (t => [t, t, t])(tvar())))
  const tprops = {}
  const inferTop = (node, env) => {
    const inf = node => inferTop(node, env)
    const wrap = args => (a => ({a, env: Object.assign(Object.fromEntries(a.map(x => [x[0], () => x[1]])), env)}))(to_a(args).map(a => [a, tvar()]))
    const to_a = o => !Array.isArray(o) ? [o] : o[0] == ',' ? o.slice(1) : o
    const to_s = a => Array.isArray(a) && a.length === 1 ? to_s(a[0]) : a
    const to_v = f => typeof f === 'function' && f.length === 0 ? f() : f
    const unify = (l, r, f) => {
      l = prune(l)
      r = prune(r)
      const narrow = (ts, target) => ts.find(t => t.toString() === target.toString()) || failInfer(`Not compatible '${ts}' '${target}'`, ts.map(t => t.toString()).join('|'), target)
      const ret = Array.isArray(l) && l[0].repeatable && unify(l[0], r, () => false) ? (Array.isArray(l[0]) ? [...l[0].slice(1), l] : (prune(l[0]), l)) :
        Array.isArray(l) && Array.isArray(r) && l.length === r.length ? l.map((t, i) => unify(t, r[i])) :
        Array.isArray(l) ? (unify(l[0], r), l.length === 2 ? l[1] : l.slice(1)) :
        l.tid && r.tid ? r.instane = l :
        l.tid ? l.instance = r :
        r.tid ? r.instance = l :
        !Array.isArray(l) && !Array.isArray(r) && l.toString() === r.toString() ? l :
        l.name in tclasses && !Array.isArray(r) ? l.instance = narrow(tclasses[l.name], r) :
        r.name in tclasses && !Array.isArray(l) ? r.instance = narrow(tclasses[r.name], l) :
        f ? f() : failInfer(`Unmatch ${l} and ${r}`, l, r)
      ret.errors = merge([...errors(ret), ...errors(l), ...errors(r)])
      return ret
    }
    const errors = t => t.errors || []
    const merge = a => (s => a.filter(v => (k => s.has(k) ? false : s.add(k))(v.toString())))(new Set())
    const switch_ = (t, cs) => switch__(t, cs.map(c => [x => Array.isArray(c[2]) ? inferTop(x, wrap(c[2].slice(1)).env) : inf(x), ...c.slice(1)]))
    const switch__ = (t, cs) => (cs.map(c => unify(t, c[0](c[2]))), cs.reduce((r,c) => unify(r, c[0](c.slice(3))), tvar()))
    const flat = a => Array.isArray(a) && a.length == 1 && a[0][0] == '__do' ? a[0].slice(1).map(flat) : a
    const struct = (name, fields) => (tprops[name] = fields, [...fields.map(f => type(f[1])), type(name)])
    const adt = (t, fields) => fields.map(f => Array.isArray(f) ? tput(f[0], () => [...f.slice(1).map(x => type(x)), t]) : tenv[f] = t)
    const squash = x => Array.isArray(x) && x[0].repeatable ? to_s(squash(x.slice(1))) : x
    const def = (args, types) => (x => types.map(t => (y => y ? y[1] : type(t.toString()))(x.a.find(y => y[0].toString() == t.toString()))))(wrap(args))
    const fn = (a, exp) => (x => [...x.a.map(y => y[1]), inferTop(exp, x.env)])(wrap(a))
    const derepeat = a => Array.isArray(a) && a[0].repeatable ? to_s(derepeat(a.slice(1))) : a
    const _do = ts => (t => (t.errors = merge(ts.flatMap(errors)), t))(ts.slice(-1)[0])
    const apply = ([head, ...argv]) =>
      head == ':' && argv[0] == 'struct' ? put(argv[1], struct(argv[1], flat(argv.slice(2)))) :
      head == ':' && argv[0] == 'adt' ? (t => (put(t.name, t), adt(t, flat(argv.slice(2)))))(type(argv[1])) :
      head == ':' && argv[0] == 'switch' ? switch_(inf(argv[1]), flat(argv.slice(2))) :
      head == ':' && argv[0] == 'def' ? (a => put(a[0], def(a.slice(1), to_a(argv[2]))))(to_a(argv[1])) :
      head == ':' && argv[0] == 'fn' ? ((id, ft) => id in env ? unify(get(id), ft) : put(id, ft))(to_a(argv[1])[0].toString(), fn(to_a(argv[1]).slice(1), argv[argv.length - 1])) :
      head == '__call' && argv.length === 1 ? to_s(squash(value(argv[0]))) :
      head == 'let' ? put(argv[0], inf(to_s(argv.slice(1)))) :
      head == 'var' ? put(argv[0], variable(inf(to_s(argv.slice(1))))) :
      head == 'tuple' ? type('tuple', ...argv.map(inf)) :
      head == '.' && argv[1].match(/^[0-9]+$/) ? inf(argv[0]).generics[argv[1]] :
      head == '__do' ? _do(argv.map(inf)) :
      head == '=>' ? (x => [...x.a.map(y => y[1]), inferTop(argv[1], x.env)])(wrap(to_a(argv[0]))) :
      head == '.' ? Object.fromEntries(tprops[inf(argv[0]).name])[argv[1]] :
      head == '!' ? unify(inf(argv[0]), tbool) :
      head == '=' ? unify(get(argv[0]), inf(to_s(argv.slice(1)))) :
      '+= -= *= /= %= **='.split(' ').includes(head.toString()) ? unify(assertVariable(get(argv[0])), inf(to_s(argv.slice(1))))  :
      derepeat(argv.reduce((ret, x) => unify(ret, inf(x)), inf(head)))
    const value = v => v.type = v.match(/^[0-9]+$/) ? tclass('num') :
      v.match(/^[0-9]+\.[0-9]+$/) ? tfloat :
      v.startsWith('r"') ? tregexp :
      v.startsWith('$"') ? tstring :
      v.startsWith('"') ? tstring :
      v.startsWith('`') ? tstring :
      get(v)
    const get = k => k in env ? to_v(env[k]) : fail(`Notfound '${k}' in env='${Object.keys(env)}'`)
    const put = (k, v) => env[k] = v
    return node.type = Array.isArray(node) ? apply(node) : value(node)
  }
  return prune(inferTop(root, tenv))
}
const fix = o => (o.type && (o.type = o.type.toString()), Array.isArray(o) && o.map(fix), o)
const convert = root => (infer(root), fix(root))

module.exports = { convert }

if (require.main === module) {
  const { parse } = require('./parse.js')
  const assert = (expect, fact, src) => put(expect === fact ? '.' : fail(`Expect: '${expect}' but got '${fact}'. src='${src}'`))
  const test = (expect, src) => assert(expect, str(infer(parse(src))), src)
  const error = (l, r, src) => {
    let ret
    try {
      ret = infer(parse(src))
    } catch (e) {
      assert(l, str(e.l), src)
      assert(r, str(e.r), src)
      return
    }
    fail(`Expect: '${l}' is not '${r}' but got '${ret}'. src=${src}`)
  }

  // primitives
  test('bool', 'true')
  test('bool', 'false')
  test('num', '1')
  test('float', '1.2')
  test('string', '"hi"')
  test('string', '`hi`')
  test('regexp', 'r"hi"')
  test('string', '$"hi"')
  test('(1 1)', 'a => a')
  test('(1 2 1)', 'a,b => a')
  test('(1 2 2)', 'a,b => b')

  // containers
  test('list(1)', '[]')
  test('list(num)', '[1]')
  test('list(float)', '[1.0]')
  test('list(num)', '[1 2]')
  test('list(float)', '[1 2.0]')
  test('list(float)', '[1 2.0 3]')
  test('set(1)', 'set()')
  test('set(num)', 'set(1)')
  test('dict(1 2)', 'dict()')
  test('dict(string num)', 'dict("a" 1)')
  test('tuple(num)', 'tuple(1)')
  test('tuple(num float)', 'tuple(1 0.0)')
  test('tuple(num float string)', 'tuple(1 0.0 "a")')

  // property
  test('num', 'tuple(1).0')

  // type cast
  test('int', 'int(1)')
  test('float', 'float(1)')
  test('int', 'int(float(1))')
  test('float', 'float(int(1))')

  // user defined variables
  test('num', 'let x 1')
  test('var(num)', 'var x 1')
  test('var(num)', 'var x 1\nx = 2')

  // user defined function
  test('(num)', 'fn f: 1')
  test('(1 1)', 'fn f x: x')
  test('(num num)', 'fn f x: x + 1')

  // user defined struct
  test('item', 'struct item:\n  name string\nitem("moa")')
  test('item', 'struct item:\n  name string\n  price int\nitem("moa" 1)')
  test('string', 'struct item:\n  name string\n  price int\nitem("moa" 1).name')
  test('int', 'struct item:\n  name string\n  price int\nitem("moa" 1).price')

  // user defined algeblaic data type
  test('ab', 'adt ab:\n  a\n  b\na')
  test('ab', 'adt ab:\n  a\n  b\nb')
  test('ab', 'adt ab:\n  a string\n  b int\na "a"')
  test('ab', 'adt ab:\n  a string\n  b int\nb 1')
  test('float', 'adt ab:\n  a\n  b\nswitch a:\n  case a: 1\n  case b: 2.0')
  test('string', 'adt ab:\n  a string\n  b int\nswitch a "hi":\n  case a s: s\n  case b n: string(n)')

  // declare type
  test('(int)', 'def f: int')
  test('int', 'def f: int\nf()')
  test('(int)', 'def f: int\nfn f: 1')
  test('(int int)', 'def f: int int')
  test('(int int)', 'def f: int int\nfn f a: a')
  test('(1 1)', 'def f a: a a')
  test('float', 'def f a: a a\nf(1) + f(1.0)')
  test('(1 2)', 'def f a b: a b')
  test('(2 1)', 'def f a b: b a')

  // single operator
  test('bool', '!true')

  // binary operators and type class
  test('num', '1 + 2')
  test('float', '1.0 + 2.0')
  test('float', '1 + 2.0')
  test('float', '1.0 + 2')
  test('float', '1 + 2 + 3.0')
  test('float', '1.0 + 2.0 + 3')
  test('string', 'string(1)')

  // test for exported function
  assert('num', convert(parse('1 + 2')).type, '1 + 2')

  // error handling
  test('2|string', 'throw "s"')
  test('num|string', '(throw "s") + (throw "m")')
  //test('(1 1|string)', 'fn f a:\n  throw "x"\n  a')

  // be failed
  error('int|float', 'string', '1 + "s"')
  error('num', '(num)', 'fn f: 1\n1+f')
  error('var(num)', 'num', 'let x 1\nx+=1')

  puts('ok')
}
