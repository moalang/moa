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
  const flat = a => Array.isArray(a) && a.length == 1 && a[0][0] == '__do' ? a[0].slice(1).map(flat) : a
  const value = x => x in map ? map[x] :
    x.startsWith('r"') ? `(new RegExp(${x.slice(1)}))` :
    x.toString()
  const apply = ([h,...t]) =>
    h == 'list' ? `[${t.map(js).join(',')}]` :
    h == 'set' ? `[${t.map(js).join(',')}]` :
    h == 'tuple' ? `[${t.map(js).join(',')}]` :
    h == 'dict' ? '({' + [...Array(t.length / 2).keys()].map(i => `[${t[i*2]}]: ${compile(t[i*2+1])}`).join(', ') + '})' :
    h == '.' && t[1].match(/^[0-9]+$/) ? `${compile(t[0])}[${t[1]}]` : // tuple(...).1
    h == '__do' ? t.map(js).join('\n') :
    h == ':' && t[0] == 'struct' ? struct(t[1], flat(t.slice(2))) :
    h == '.' ? compile(t[0]) + '.' + t[1] :
    `${h}(${t.map(js).join(',')})`
  const js = x => Array.isArray(x) ? (x in map ? map[x] : apply(x)) : value(x)
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

  puts('ok')
}
