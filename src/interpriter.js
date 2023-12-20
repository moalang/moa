/*
 * Eval internal expression
 */
const { parse } = require('./parser.js')
class List extends Array { }
class Return { constructor(value) { this.value = value } }
const string = o => typeof o === 'string' ? o : literal(o)
const literal = o =>
  o instanceof RegExp ? o.toString() :
  o instanceof Error ? `Error(${o.message})` :
  typeof o === 'function' ? o.toString() :
  typeof o === 'object' && o.constructor === Map ? JSON.stringify(Object.fromEntries([...o])) :
  Array.isArray(o) ? `[${o.map(literal).join(' ')}]` :
  JSON.stringify(o)
const log = o => { console.dir(o, {depth: null}); return o }
const fail = (...a) => { throw new Error(a.map(string).join(' ')) }
const put = (...a) => { process.stdout.write(a.map(string).join(' ')); return a[0] }
const puts = (...a) => { console.log(a.map(string).join(' ')); return a[0] }
const attempt = (f, g) => { try { return f() } catch (e) { return g(e) } }
const range = (n, m) => [...Array(n)].map((_, i) => i + (m || 0))
const index = (a, i) =>
  i >= 0 && i < a.length ? a[i] :
  i < 0 && a.length + i >= 0 && a.length + i < a.length ? a[a.length + i] :
  fail('OutOfIndex', i)
const comparable = x => "'" + JSON.stringify(_comparable(x)) + "'"
const _comparable = x => Array.isArray(x) ? x.map(_comparable) :
  x instanceof Map ? [...x.keys()].sort().map(key => _comparable(x.get(key))) :
  typeof x === 'object' ? Object.keys(x).sort().map(key => _comparable(x[key])) :
  typeof x === 'number' ? (Array(16).join('0') + x).slice(-16) :
  x
const embedded = {
  '!': b => !b,
  '-': (l, r) => r === undefined ? -l : l - r,
  '+': (l, r) => l + r,
  '*': (l, r) => l * r,
  '/': (l, r) => r === 0 ? fail('ZeroDivision') : l / r,
  '%': (l, r) => r === 0 ? fail('ZeroDivision') : l % r,
  '**': (l, r) => l ** r,
  '<': (l, r) => comparable(l) < comparable(r),
  '<=': (l, r) => comparable(l) <= comparable(r),
  '>': (l, r) => comparable(l) > comparable(r),
  '>=': (l, r) => comparable(l) >= comparable(r),
  '==': (l, r) => comparable(l) === comparable(r),
  '!=': (l, r) => comparable(l) !== comparable(r),
  '++': (l, r) => l instanceof Map ? new Map([...l, ...r]) : l.concat(r),
  list: (...a) => new List().concat(a),
  dict: (...a) => new Map(range(a.length / 2).map(i => [a[i*2], a[i*2+1]])),
  __call: f => f(),
  __index: index,
}
const evaluate = (node, obj) => execute(node, Object.fromEntries(Object.entries(obj).map(([key, value]) => [key, {value}])))
const execute = (node, env) => {
  const run = node => Array.isArray(node) ? apply(node) : unit(node)
  const prop = (obj, key) => key in obj ? obj[key] : fail('Missing', key, 'of', Object.keys(obj))
  const lookup = key => key in env ? env[key].value : fail('Missing', key, 'in', Object.keys(env))
  const insert = (key, value) => key in env ? fail('Existed', key) : env[key] = {value}
  const update = (key, value) =>
    Array.isArray(key) && key[0] === '.' && key[1] in env ? env[key[1]].value[key[2]] = value :
    key in env ? env[key].value = value : fail('Missing', key)
  const make_func = (keys, body) => (...values) => execute(body, {...env, ...  Object.fromEntries(to_array(keys).map((key, i) => [key, {value: values[i]}]))})
  const def_class = a => (keys => (...values) => Object.fromEntries(keys.map((key, i) => [key, values[i]])))(a[0] === '__pack' ? a.slice(1).map(([name, type]) => name) : [a[0]])
  const pack = ([x, ...xs]) => (x => x instanceof Return ? x.value : xs.length ? pack(xs) : x)(run(x))
  const apply = a =>
    a.length === 1 ? run(a[0]) :
    a[0] === '=>' ? make_func(a[1], a.slice(2)) :
    a[0] === 'let' ? insert(a[1], run(a.slice(2))) :
    a[0] === 'var' ? insert(a[1], run(a.slice(2))) :
    a[0] === 'def' ? insert(a[1], make_func(a.slice(2, -1), index(a, -1))) :
    a[0] === 'test' ? make_func(a[1], a[2])({eq: (a, b) => comparable(a) === comparable(b) || fail('ne', literal(a), literal(b))}) :
    a[0] === 'class' ? insert(a[1], def_class(a[2])) :
    a[0] === '__pack' ? pack(a.slice(1)) :
    a[0] === 'guard' ? run(a[1]) && new Return(run(a[2])) :
    a[0] === '&&' ? run(a[1]) && run(a[2]) :
    a[0] === '||' ? run(a[1]) || run(a[2]) :
    a[0] === ':=' ? update(a[1], run(a.slice(2))) :
    a[0] === '.' ? prop(run(a[1]), a[2]) :
    a[0].toString().match(/^[+\-*/%|&]+=$/) ? update(a[1], apply([a[0].slice(0, -1), ...a.slice(1)])) :
    a.length > 1 ? run(a[0])(...a.slice(1).map(run)) :
    fail('apply', a)
  const to_array = o => Array.isArray(o) ? o : [o]
  const unit = s =>
    typeof s !== 'string' ? s : // already evaluated
    s === 'true' ? true :
    s === 'false' ? false :
    s.startsWith('r"') ? new RegExp(s.slice(2, -1), 'g') :
    s.startsWith('"') ? JSON.parse(s.replace(/\n/g, '\\n').replace(/\t/g, '\\t')) :
    s.match(/^-?[0-9]/) ? parseFloat(s) :
    s in embedded ? embedded[s] : lookup(s)
  return run(node)
}

module.exports = { evaluate }
if (require.main === module) {
  const eq = (expect, src) => {
    const actual = attempt(() => evaluate(parse(src), {}), e => e)
    if (comparable(expect) === comparable(actual)) {
      put('.')
    } else {
      puts()
      puts(`Expect: ${string(expect)}`)
      puts(`Actual: ${string(actual)}`)
      puts(`Source: ${JSON.stringify(src)}`)
      put('Nodes : ')
      log(parse(src))
      throw Error('Test failed')
    }
  }
  const test = (expect, src) => eq(expect, src)

  // primitives
  test(1, '1')
  test(-1, '-1')
  test(true, 'true')
  test(false, 'false')
  test('hi', '"hi"')
  test('h"i', '"h\\"i"')
  test('\\', '"\\\\"')
  test('\t', '"\\t"')
  test('\\t', '"\\\\t"')
  test('\\\t', '"\\\\\\t"')
  test(/[a-z]\t/g, 'r"[a-z]\\t"')
  test(1, '(a => a)(1)')
  test(3, '(a,b => a + b)(1 2)')
  test('s', '(a => a)("s")')

  // containers
  test([], '[]')
  test([1], '[1]')
  test([1,2], '[1 2]')
  test(1, '[1][0]')
  test(2, '[1 2][-1]')
  test({}, 'dict()')
  test({a: 1}, 'dict("a" 1)')

  // define
  test(1, 'def f: 1\nf()')
  test(1, 'def f a: a\nf(1)')
  test(0, 'var a 0\ndef f a:\n  a += 1\nf(a)\na') // local scope
  test(1, 'var a 0\ndef f: a += 1\nf()\na') // outer scope
  test(2, 'def f a:\n  let m a % 2\n  guard m == 0 a\n  f(a - 1)\nf(3)') // recursion

  // statement
  test(2, '1\n2')
  test(2, 'guard false: 1\n2')
  test(1, 'guard true: 1\n2')
  test(1, 'guard 1 == 1:\n  1\n2')
  test(2, 'guard 1 == 2:\n  1\n2')

  // class
  test('hi', 'class s:\n  a string\ns("hi").a')
  test(1, 'class s:\n  a string\n  b int\ns("hi" 1).b')
  test({a: "hi", b: 1}, 'class s:\n  a string\n  b int\ns("hi" 1)')

  // test
  test(true, 'test t: t.eq 1 1')
  test(Error('ne 1 2'), 'test t: t.eq 1 2')
  test(Error('ne "a" "b"'), 'test t: t.eq "a" "b"')

  // operators
  test(false, '!true')
  test(true, 'true && !false')
  test(false, 'true && false')
  test(true, 'true && true')
  test(true, 'true || false')
  test(false, '1 < 1')
  test(true, '1 <= 1')
  test(false, '1 > 1')
  test(true, '1 >= 1')
  test(true, '1 == 1')
  test(false, '1 != 1')
  test(3, '1 + 2')
  test(1, '3 - 2')
  test(6, '2 * 3')
  test(1.5, '3 / 2')
  test(1, '3 % 2')
  test(27, '3 ** 3')
  test(3, 'var a 1\na += 2\na')
  test(2, 'var a 1\na := 2\na')
  test(true , 'class s:\n  a string\n  b int\ns("hi" 1) == s("hi" 1)')
  test(false, 'class s:\n  a string\n  b int\ns("hi" 1) == s("hi" 2)')
  test(true , 'class s:\n  a string\n  b int\ns("hi" 1) <= s("hi" 1)')
  test(false, 'class s:\n  a string\n  b int\ns("hi" 1) < s("hi" 1)')
  test(true , 'class s:\n  a string\n  b int\ns("hi" 1) < s("hi" 2)')
  test(true , 'class s:\n  a string\n  b int\ns("hi" 9) < s("hi" 10)')
  test(false, 'class s:\n  a string\n  b int\ns("hi" 10)< s("hi" 9)')
  test(2, 'class c:\n  a int\nvar x c 1\nx.a := 2')
  test(2, 'class c:\n  a int\nvar x c 1\nx.a := 2\nx.a')
  test('ab', '"a" ++ "b"')
  test([1, 2], '[1] ++ [2]')
  test([[1], [2]], '[[1]] ++ [[2]]')
  test({a: 1, b: 3, c: 4}, 'dict("a" 1 "b" 2) ++ dict("b" 3 "c" 4)')

  // methods
  //test(2, '"hi".size')
  //test(['', '1', '', '2', ''], '"12".rsplit(r"([0-9])")')
  //test('h', '"hi"[0]')
  //test(1, '"1".int')
  //test(1.1, '"1.1".float')
  //test(['a', 'b'], '"a b".split(" ")')
  //test('"\\t"', 'string("\\t")')
  //test('"\\\\t"', 'string("\\\\t")')
  //test('"\\\\\\t"', 'string("\\\\\\t")')
  //'1 -1 0.1 true "s" [] 0,1'.split(' ').map(s => test(s, `string(${s})`))
  //test(0, '[].size')
  //test(Error('0 exceeded'), '[][0]')
  //test(0, '[].size')
  //test(2, '[1 2].size')
  //test([2], '[1 2].keep x => x > 1')
  //test(0, '[1].index 1')
  //test(-1, '[1].index 2')
  //test("a", '["a"].zip(["1"])[0].0')
  //test(1, '["a"].zip(["1"])[0].1')
  //test({a: 1}, '["a",1].dict()')
  //test(true, '[].all(x => x == 1)')
  //test(true, '[1].all(x => x == 1)')
  //test(false, '[2].all(x => x == 1)')
  //test({a:2}, 'dict("a" 1).vmap(n => n + 1)')
  //test(1, 'dict("a" 1)["a"]')
  //test({a:1}, 'let d dict()\nd["a"]:=1\nd')

  // error
  test(Error('Existed z'), 'let z 1\nlet z 2')
  test(Error('ZeroDivision'), '3 / 0')
  test(Error('Missing z'), 'z := 1')
  test(Error('OutOfIndex 0'), '[][0]')
  test(Error('OutOfIndex -2'), '[0][-2]')

  // edge case
  test(1, 'var a 0\ndef f:\n  def g: a += 1\n  g()\nf()\na')

  puts('ok')
}
