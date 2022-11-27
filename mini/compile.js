/*
 * This program generate JavaScript from an internal expression.
 * [x] Primitives
 * [x] Containers
 */
const dump = o => { console.dir(o, {depth: null}); return o }
const fail = m => { throw new Error(m) }
const str = o =>
  typeof o === 'string' ? o :
  Array.isArray(o) ? `(${o.map(str).join(' ')})` :
  JSON.stringify(o)
const put = (...a) => { process.stdout.write(a.map(str).join(' ')); return a[0] }
const puts = (...a) => { console.log(a.map(str).join(' ')); return a[0] }

const compile = root => {
  const map = {
    '__call,list': '[]',
    '__call,set': '[]',
    '__call,dict': '({})',
  }
  const struct = (name, fields) => `const ${name} = (${fields.map(f => f[0])}) => ({${fields.map(f => f[0])}})`
  const adt = (name, fields) => fields.map(f => Array.isArray(f) ? adtValue(f[0], f[1]) : adtTag(f)).join('\n') + `\nconst ${name} = ({${fields.map(f => f[0])}})`
  const adtTag = name => `const ${name} = ({__tag: '${name}'})`
  const adtValue = (name, v) => `const ${name} = __val => ({__tag: '${name}', __val})`
  const flat = a => Array.isArray(a) && a.length == 1 && a[0][0] == '__do' ? a[0].slice(1).map(flat) : a
  const switch_ = (t, cs) => `(__target => ${cs.map(c => Array.isArray(c) ? caseValue(c) : caseTag(c)).join(' : ')} : (() => {throw new Error("switch unmatch")})() )(${js(t)})`
  const caseValue = c => `__target.__tag === '${c[2][0]}' ? (${c[2][1]} => ${js(c[3])})(__target.__val)`
  const caseTag = c => `__target.__tag === '${c[2]}' ? ${js(c[3])}`
  const value = x => x in map ? map[x] :
    x.startsWith('r"') ? `(new RegExp(${x.slice(1)}))` :
    x.toString()
  const apply = ([h,...t]) =>
    h == 'list' ? `[${t.map(js).join(',')}]` :
    h == 'string' ? `(${js(t)}).toString()` :
    h == 'set' ? `[${t.map(js).join(',')}]` :
    h == 'tuple' ? `[${t.map(js).join(',')}]` :
    h == 'dict' ? '({' + [...Array(t.length / 2).keys()].map(i => `[${t[i*2]}]: ${compile(t[i*2+1])}`).join(', ') + '})' :
    h == '.' && t[1].match(/^[0-9]+$/) ? `${compile(t[0])}[${t[1]}]` : // tuple(...).1
    h == '__do' ? t.map(js).join(';\n') :
    h == ':' && t[0] == 'struct' ? struct(t[1], flat(t.slice(2))) :
    h == ':' && t[0] == 'adt' ? adt(t[1], flat(t.slice(2))) :
    h == ':' && t[0] == 'switch' ? switch_(js(t[1]), flat(t.slice(2))) :
    h == '.' ? compile(t[0]) + '.' + t[1] :
    `${h}(${t.map(js).join(',')})`
  //const js = x => Array.isArray(x) ? (x in map ? map[x] : apply(x)) : value(x)
  const js = x => !Array.isArray(x) ? value(x) : 
    x.length === 1 ? js(x[0]) :
    x in map ? map[x] :
    apply(x)
  return js(root)
}

module.exports = { compile }

if (require.main === module) {
  const { parse } = require('./parse.js')
  const { convert } = require('./convert.js')
  const assert = (expect, fact, src) => put(expect === fact ? '.' : fail(`Expect: '${expect}' but got '${fact}'. src='${src}'`))
  const test = (expect, src) => assert(str(expect), str(eval(compile(convert(parse(src))))), src)

  // primitives
  test(true, 'true')
  test(false, 'false')
  test(1, '1')
  test(1.2, '1.2')
  test('1', 'string(1)')
  test('hi', '"hi"')
  test('hi', '`hi`')
  test(/hi/, 'r"hi"')

  // containers
  test([], '[]')
  test([1], '[1]')
  test([1,2], '[1 2]')
  test([], 'set()')
  test([1], 'set(1)')
  test({}, 'dict()')
  test({s:1}, 'dict("s" 1)')
  test({1:2}, 'dict(1 2)')
  test([1,2], 'tuple(1 2)')
  test(1, 'tuple(1 2.0).0')
  test(2.0, 'tuple(1 2.0).1')

  // user defined type
  test('moa', 'struct item:\n  name string\nitem("moa").name')
  test(1, 'struct item:\n  name string\n  price int\nitem("moa" 1).price')

  // user defined algebraic data type
  test(1, 'adt ab:\n  a\n  b\nswitch a:\n  case a: 1\n  case b: 2')
  test('hi', 'adt ab:\n  a string\n  b int\nswitch a "hi":\n  case a s: s\n  case b n: string(n)')
  test('hi', 'adt ab:\n  a string\n  b int\nswitch a "hi":\n  case a s: s\n  case b n: string(n)')
  test('1', 'adt ab:\n  a string\n  b int\nswitch b 1:\n  case a s: s\n  case b n: string(n)')

  puts('ok')
}
