/*
 * This program converts some nodes in an internal expression based on type inference.
 * [x] Basic type inference
 * [ ] Convert a method call to a function call
 */
const dump = o => { console.dir(o, {depth: null}); return o }
const fail = m => { throw new Error(m) }
const str = o => Array.isArray(o) && o.length === 1 ? _str(o[0]) : _str(o)
const _str = o => typeof o === 'string' ? o :
  o instanceof String ? o.toString() :
  Array.isArray(o) ? `(${o.map(_str).join(' ')})${o.repeatable ? '*' : ''}` :
  o.instance ? _str(o.instance) :
  o.tid || o.type || o.name ? o.toString() + (o.repeatable ? '*' : '') :
  JSON.stringify(o)
const put = (...a) => { process.stdout.write(a.map(str).join(' ')); return a[0] }
const puts = (...a) => { console.log(a.map(str).join(' ')); return a[0] }
const infer = root => {
  let unique = 1
  const repeat = t => (t.repeatable = true, t)
  const tvar = () => (tid => ({tid, instance: null, toString: () => tid.toString()}))(unique++)
  const tclass = name => ({name, instance: null, toString: () => name})
  const type = (name, ...generics) => ({name, generics, toString: () => `${name}${generics.length ? `(${generics.map((g, i) => g.instance ? g.instance.toString() : g.toString()).join(' ')})` : ''}`})
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
    int: () => tint,
    'true': () => tbool,
    'false': () => tbool,
    'string': () => [tvar(), tstring],
    'int': () => [type('num'), tint],
    'float': () => [type('num'), tfloat],
    'list': () => (t => [t, type('list', t)])(repeat(tvar())),
    'set': () => (t => [t, type('set', t)])(repeat(tvar())),
    'dict': () => (a => [a, type('dict', a[0], a[1])])(repeat([tvar(), tvar()])),
  }
  '+ - * / % ** //'.split(' ').map(op => tenv[op] = () => (t => [t, t, t])(type('num')))
  '&& ||'.split(' ').map(op => tenv[op] = () => [tbool, tbool, tbool])
  '== != < <= > >='.split(' ').map(op => tenv[op] = () => (t => [t, t, t])(tvar()))
  const tprops = {}
  const inferTop = (node, env) => {
    const inf = node => inferTop(node, env)
    const wrap = args => (a => ({a, env: Object.assign(Object.fromEntries(a.map(x => [x[0], () => x[1]])), env)}))(to_a(args).map(a => [a, tvar()]))
    const to_a = o => !Array.isArray(o) ? [o] : o[0] == ',' ? o.slice(1) : o
    const unify = (l, r, f) => {
      l = prune(l)
      r = prune(r)
      const narrow = (ts, target) => ts.find(t => t.toString() === target.toString()) || fail(`Not compatible '${ts}' '${target}'`)
      const ret = Array.isArray(l) && l[0].repeatable && unify(l[0], r, () => false) ? (Array.isArray(l[0]) ? [...l[0].slice(1), l] : (prune(l[0]), l)) :
        Array.isArray(l) ? (unify(l[0], r), l.length === 2 ? l[1] : l.slice(1)) :
        l.tid && r.tid ? r.instane = l :
        l.tid ? l.instance = r :
        r.tid ? r.instance = l :
        l.toString() === r.toString() ? l :
        l.name in tclasses ? l.instance = narrow(tclasses[l.name], r) :
        r.name in tclasses ? r.instance = narrow(tclasses[r.name], l) :
        f ? f() : fail(`Unmatch ${l} and ${r}`)
      return ret
    }
    const switch_ = (t, cs) => switch__(t, cs.map(c => [x => Array.isArray(c[2]) ? inferTop(x, wrap(c[2].slice(1)).env) : inf(x), ...c.slice(1)]))
    const switch__ = (t, cs) => (cs.map(c => unify(t, c[0](c[2]))), cs.reduce((r,c) => unify(r, c[0](c.slice(3))), tvar()))
    const flat = a => Array.isArray(a) && a.length == 1 && a[0][0] == '__do' ? a[0].slice(1).map(flat) : a
    const struct = (name, fields) => (tprops[name] = fields, [...fields.map(f => type(f[1])), type(name)])
    const adt = (t, fields) => fields.map(f => Array.isArray(f) ? tenv[f[0]] = () => [...f.slice(1).map(x => type(x)), t] : tenv[f] = () => t)
    const squash = x => Array.isArray(x) && x[0].repeatable ? squash(x.slice(1)) : x
    const apply = ([head, ...argv]) => head == '__call' && argv.length === 1 ? squash(value(argv[0])) :
      head == 'tuple' ? type('tuple', ...argv.map(inf)) :
      head == '.' && argv[1].match(/^[0-9]+$/) ? inf(argv[0]).generics[argv[1]] :
      head == '__do' ? argv.map(inf).slice(-1)[0] :
      head == ':' && argv[0] == 'struct' ? env[argv[1]] = () => struct(argv[1], flat(argv.slice(2))) :
      head == ':' && argv[0] == 'adt' ? (t => (env[t.name] = t, adt(t, flat(argv.slice(2)))))(type(argv[1])) :
      head == ':' && argv[0] == 'switch' ? switch_(inf(argv[1]), flat(argv.slice(2))) :
      head == '=>' ? (x => [...x.a.map(y => y[1]), inferTop(argv[1], x.env)])(wrap(to_a(argv[0]))) :
      head == '.' ? Object.fromEntries(tprops[inf(argv[0]).name])[argv[1]] :
      head == '!' ? unify(inf(argv[0]), tbool) :
      derepeat(argv.reduce((ret, x) => unify(ret, inf(x)), inf(head)))
    const derepeat = a => Array.isArray(a) && a[0].repeatable ? derepeat(a.slice(1)) : a
    const value = v => v.type = v.match(/^[0-9]+$/) ? tclass('num') :
      v.match(/^[0-9]+\.[0-9]+$/) ? tfloat :
      v.startsWith('r"') ? tregexp :
      v.startsWith('"') ? tstring :
      v.startsWith('`') ? tstring :
      v in env ? env[v]() :
      fail(`Unknown value '${v}' on env='${Object.keys(env)}'`)
    return node.type = Array.isArray(node) ? apply(node) : value(node)
  }
  return prune(inferTop(root, tenv))
}
const fix = o => (Array.isArray(o) ? o.map(fix) : o.type && (o.type = o.type.toString()), o)
const convert = root => (infer(root), fix(root))

module.exports = { convert }

if (require.main === module) {
  const { parse } = require('./parse.js')
  const assert = (expect, fact, src) => put(expect === fact ? '.' : fail(`Expect: '${expect}' but got '${fact}'. src='${src}'`))
  const test = (expect, src) => assert(expect, str(infer(parse(src))), src)

  // primitives
  test('bool', 'true')
  test('bool', 'false')
  test('num', '1')
  test('float', '1.2')
  test('string', '"hi"')
  test('string', '`hi`')
  test('regexp', 'r"hi"')
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

  // binary operators and type class
  test('num', '1 + 2')
  test('float', '1.0 + 2.0')
  test('float', '1 + 2.0')
  test('float', '1.0 + 2')
  test('float', '1 + 2 + 3.0')
  test('float', '1.0 + 2.0 + 3')
  test('string', 'string(1)')

  // single operator
  test('bool', '!true')

  // test for exported function
  assert('num', convert(parse('1 + 2')).type.toString(), '1 + 2')

  puts('ok')
}
