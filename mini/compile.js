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
  }
  const value = x => x in map ? map[x] : x.toString()
  const apply = ([h,...t]) =>
    h == 'list' ? `[${t.map(js).join(',')}]` :
    h == 'set' ? `[${t.map(js).join(',')}]` :
    `${h}(${t.map(js).join(',')})`
  const js = x => Array.isArray(x) ? (x in map ? map[x] : apply(x)) : value(x)
  return js(root)
}

module.exports = { compile }

if (require.main === module) {
  const { parse } = require('./parse.js')
  const { convert } = require('./convert.js')
  const assert = (expect, fact, src) => put(str(expect) === str(fact) ? '.' : fail(`Expect: '${expect}' but got '${fact}'. src='${src}'`))
  const test = (expect, src) => assert(expect, eval(compile(convert(parse(src)))), src)

  // primitives
  test(true, 'true')
  test(false, 'false')
  test(1, '1')
  test(1.2, '1.2')
  test('hi', '"hi"')
  test('hi', '`hi`')

  // containers
  test([], '[]')
  test([1], '[1]')
  test([1,2], '[1 2]')
  test([], 'set()')
  test([1], 'set(1)')

  puts('ok')
}
