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
  const to_a = o => Array.isArray(o) ? o : [o]
  const to_s = o => Array.isArray(o) && o.length === 1 ? o[0] : o
  const map = {
    '__call,list': '[]',
    '__call,set': '[]',
    '__call,dict': '({})',
  }
  const struct = (name, fields) => `const ${name} = (${fields.map(f => f[0])}) => ({${fields.map(f => f[0])}, toString() { return '${name}(' + ${fields.map(f => f[1] == 'string' ? 'JSON.stringify(' + f[0] + ')' : '(' + f[0] + ').toString()').join(" + ' ' + ")} + ')' }})`
  const adt = (name, fields) => fields.map(f => Array.isArray(f) ? adtValue(f[0], f[1]) : adtTag(f)).join('\n') + `\nconst ${name} = ({${fields.map(f => f[0])}})`
  const adtTag = name => `const ${name} = ({__tag: '${name}', toString() { return '${name}' }})`
  const adtValue = (name, v) => `const ${name} = __val => ({__tag: '${name}', __val, toString() { return '${name}(' + ${v == 'string' ? 'JSON.stringify(__val)' : '__val.toString()'} + ')' }})`
  const flat = a => Array.isArray(a) && a.length == 1 && a[0][0] == '__do' ? a[0].slice(1).map(flat) : a
  const switch_ = (t, cs) => `(__target => ${cs.map(c => Array.isArray(c) ? caseValue(c) : caseTag(c)).join(' : ')} : (() => {throw new Error("switch unmatch")})() )(${js(t)})`
  const caseValue = c => `__target.__tag === '${c[2][0]}' ? (${c[2][1]} => ${js(c[3])})(__target.__val)`
  const caseTag = c => `__target.__tag === '${c[2]}' ? ${js(c[3])}`
  const to_return = a => Array.isArray(a) && a[0] == '__do' ? a.slice(1).map(js).slice(0, -1).join(';\n') + ';\nreturn ' + js(a.slice(1).slice(-1)[0]) : `return ${js(a)}`
  const fn = (id, args, body) => `function ${id}(${args}) { ${to_return(body)} }`
  const string = (s,t) =>
      t.startsWith('tuple(') ? `'tuple(' + ${s}.map(x => x.toString()).join(' ') + ')'` :
      t.startsWith('list(') ? `'[' + ${s}.map(x => x.toString()).join(' ') + ']'` :
      t.startsWith('set(') ? `'set(' + ${s}.map(x => x.toString()).join(' ') + ')'` :
      t.startsWith('dict(') ? `'dict(' + Object.entries(${s}).map(x => x.map(y => y.toString()).join(' ')).join(' ') + ')'` :
      `(${s}).toString()`
  const value = x => x in map ? map[x] :
    x.startsWith('r"') ? `(new RegExp(${x.slice(1)}))` :
    x.startsWith('$"') ? "`" + x.slice(2, -1).replace(/{/g, '${') + "`" :
    x.toString()
  const apply = ([h,...t]) =>
    h == 'let' ? `const ${t[0]} = ${js(...t.slice(1))}` :
    h == 'var' ? `let ${t[0]} = ${js(...t.slice(1))}` :
    h == 'list' ? `[${t.map(js).join(',')}]` :
    h == 'string' ? string(js(t[0]), t[0].type) :
    h == 'set' ? `[${t.map(js).join(',')}]` :
    h == 'tuple' ? `[${t.map(js).join(',')}]` :
    h == 'dict' ? '({' + [...Array(t.length / 2).keys()].map(i => `[${t[i*2]}]: ${compile(t[i*2+1])}`).join(', ') + '})' :
    h == '.' && t[1].match(/^[0-9]+$/) ? `${compile(t[0])}[${t[1]}]` : // tuple(...).1
    h == '__do' ? t.map(js).join(';\n') :
    h == ':' && t[0] == 'struct' ? struct(t[1], flat(t.slice(2))) :
    h == ':' && t[0] == 'adt' ? adt(t[1], flat(t.slice(2))) :
    h == ':' && t[0] == 'switch' ? switch_(js(t[1]), flat(t.slice(2))) :
    h == ':' && t[0] == 'fn' ? fn(to_a(t[1])[0], to_a(t[1]).slice(1), to_s(t.slice(2))) :
    h == '.' ? compile(t[0]) + '.' + t[1] :
    h == '!' ? '!' + js(t[0]) :
    h == '/' ? `(d => d === 0 ? (() => {throw new Error('zdiv')})() : ${js(t[0])} / d)(${js(t[1])})` :
    h == '%' ? `(d => d === 0 ? (() => {throw new Error('zdiv')})() : ${js(t[0])} % d)(${js(t[1])})` :
    h == '//' ? `(d => d === 0 ? (() => {throw new Error('zdiv')})() : Math.floor(${js(t[0])} / d))(${js(t[1])})` :
    '== != < <= > >='.split(' ').includes(h.toString()) ? `JSON.stringify(${js(t[0])}) ${h} JSON.stringify(${js(t[1])})` :
    '+-*/%<>=!|&'.includes(h[0]) ? js(t[0]) + h + js(t[1]) :
    `${h}(${t.map(js).join(',')})`
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
  const error = (expect, src) => {
    try {
      eval(compile(convert(parse(src))))
      puts('Expected error not happend')
    } catch (e) {
      assert(str(expect), e.message)
    }
  }

  // primitives
  test(true, 'true')
  test(false, 'false')
  test(1, '1')
  test(1.2, '1.2')
  test('1', 'string(1)')
  test('hi', '"hi"')
  test('hi', '`hi`')
  test(/hi/, 'r"hi"')
  test("hi 2", 'let n 1\nlet s "hi"\n$"{s} {n + 1}"')

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
  test('[]', 'string([])')
  test('[1]', 'string([1])')
  test('[1 2]', 'string([1 2])')
  test('set()', 'string(set())')
  test('set(1)', 'string(set(1))')
  test('set(1 2)', 'string(set(1 2))')
  test('dict()', 'string(dict())')
  test('dict(1 2)', 'string(dict(1 2))')
  test('tuple(1 2)', 'string(tuple(1 2))')

  // single operator
  test(false, '!true')

  // binary operators
  test(3, '1 + 2')
  test(1, '3 - 2')
  test(6, '2 * 3')
  test(1.5, '3 / 2')
  test(1, '3 % 2')
  test(27, '3 ** 3')
  test(1, '3 // 2')
  test(false, '1 < 1')
  test(true, '1 <= 1')
  test(false, '1 > 1')
  test(true, '1 >= 1')
  test(true, '1 == 1')
  test(false, '1 != 1')
  test(false, 'true && false')
  test(true, 'true && true')
  test(true, 'true || false')

  // user defined function
  test(1, 'fn f a: a\nf(1)')
  test(2, 'fn f a:\n  let b a + 1\n  b\nf(1)')
  test(2, 'fn f a:\n  var b a\n  a += 1\nf(1)')

  // user defined type
  test('hi', 'struct s:\n  a string\ns("hi").a')
  test(1, 'struct s:\n  a string\n  b int\ns("hi" 1).b')
  test(true, 'struct s:\n  a string\n  b int\ns("hi" 1) == s("hi" 1)')
  test(false, 'struct s:\n  a string\n  b int\ns("hi" 1) == s("hi" 2)')
  test(false, 'struct s:\n  a string\n  b int\ns("hi" 1) < s("hi" 1)')
  test(true, 'struct s:\n  a string\n  b int\ns("hi" 1) <= s("hi" 1)')
  test(false, 'struct s:\n  a string\n  b int\ns("hi" 1) > s("hi" 1)')
  test(true, 'struct s:\n  a string\n  b int\ns("hi" 1) >= s("hi" 1)')
  test('s("hi" 1)', 'struct s:\n  a string\n  b int\nstring(s("hi" 1))')

  // user defined algebraic data type
  test(1, 'adt ab:\n  a\n  b\nswitch a:\n  case a: 1\n  case b: 2')
  test('hi', 'adt ab:\n  a string\n  b int\nswitch a "hi":\n  case a s: s\n  case b n: string(n)')
  test('hi', 'adt ab:\n  a string\n  b int\nswitch a "hi":\n  case a s: s\n  case b n: string(n)')
  test('1', 'adt ab:\n  a string\n  b int\nswitch b 1:\n  case a s: s\n  case b n: string(n)')
  test('a', 'adt ab:\n  a\n  b int\n  c string\nstring(a)')
  test('b(1)', 'adt ab:\n  a\n  b int\n  c string\nstring(b(1))')
  test('c("hi")', 'adt ab:\n  a\n  b int\n  c string\nstring(c("hi"))')

  // error handling
  error('zdiv', '1 / 0')
  error('zdiv', '1 // 0')
  error('zdiv', '1 % 0')

  puts('ok')
}
