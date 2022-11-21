/*
 * This program converts some nodes in an internal expression based on type inference.
 * [x] Basic type inference
 * [ ] Convert a method call to a function call
 */
const dump = o => { console.dir(o, {depth: null}); return o }
const fail = m => { throw new Error(m) }
const str = o => typeof o === 'string' ? o :
  Array.isArray(o) ? `(${o.map(str).join(' ')})${o.repeatable ? '*' : ''}` :
  o.instance ? str(o.instance) :
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
    '+': () => (t => [t, t, t])(type('num')),
    'int': () => [type('num'), tint],
    'float': () => [type('num'), tfloat],
    'list': () => (t => [t, type('list', t)])(repeat(tvar())),
    'set': () => (t => [t, type('set', t)])(repeat(tvar())),
    'dict': () => (a => [a, type('dict', a[0], a[1])])(repeat([tvar(), tvar()])),
  }
  const tprops = {}
  const inferTop = (node, env) => {
    const inf = node => inferTop(node, env)
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
    const flat = a => Array.isArray(a) && a.length == 1 && a[0][0] == '__do' ? a[0].slice(1).map(flat) : a
    const struct = (name, fields) => (tprops[name] = fields, [...fields.map(f => type(f[1])), type(name)])
    const squash = x => Array.isArray(x) && x[0].repeatable ? squash(x.slice(1)) : x
    const apply = ([head, ...argv]) => head == '__call' && argv.length === 1 ? squash(value(argv[0])) :
      head == 'tuple' ? type('tuple', ...argv.map(inf)) :
      head == '.' && argv[1].match(/^[0-9]+$/) ? inf(argv[0]).generics[argv[1]] :
      head == '__do' ? argv.map(inf).slice(-1)[0] :
      head == ':' && argv[0] == 'struct' ? env[argv[1]] = () => struct(argv[1], flat(argv.slice(2))) :
      head == '.' ? Object.fromEntries(tprops[inf(argv[0]).name])[argv[1]] :
      derepeat(argv.reduce((ret, x) => unify(ret, inf(x)), inf(head)))
    const derepeat = a => Array.isArray(a) && a[0].repeatable ? derepeat(a.slice(1)) : a
    const value = v => v.type = v.match(/^[0-9]+$/) ? tclass('num') :
      v.match(/^[0-9]+\.[0-9]+$/) ? tfloat :
      v.startsWith('r"') ? tregexp :
      v.startsWith('"') ? tstring :
      v.startsWith('`') ? tstring :
      v in env ? env[v]() :
      fail(`Unknown value '${v}'`)
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
  const test = (expect, src) => assert(expect, infer(parse(src)).toString(), src)

  // primitives
  test('bool', 'true')
  test('bool', 'false')
  test('num', '1')
  test('float', '1.2')
  test('string', '"hi"')
  test('string', '`hi`')
  test('regexp', 'r"hi"')

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

  // type class
  test('num', '1 + 2')
  test('float', '1.0 + 2.0')
  test('float', '1 + 2.0')
  test('float', '1.0 + 2')
  test('float', '1 + 2 + 3.0')
  test('float', '1.0 + 2.0 + 3')

  // user defined struct
  test('item', 'struct item:\n  name string\nitem("moa")')
  test('item', 'struct item:\n  name string\n  price int\nitem("moa" 1)')
  test('string', 'struct item:\n  name string\n  price int\nitem("moa" 1).name')
  test('int', 'struct item:\n  name string\n  price int\nitem("moa" 1).price')

  // test for exported function
  assert('num', convert(parse('1 + 2')).type.toString(), '1 + 2')

  puts('ok')
}
