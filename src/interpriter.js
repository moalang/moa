// Eval internal expression

const { parse } = require('./parser.js')
class List extends Array { }
class Tuple extends Array { }
class SoftError extends Error {}
class Return { constructor(ret) { this.ret = ret } }
class Time { constructor(year, month, day, hour, minute, second) { Object.assign(this, {year, month, day, hour, minute, second}) } }
class Duration { constructor(count, unit) { this.count = count; this.unit = unit } }
class Enum { constructor(tag, content) { this.tag = tag; this.content = content } }
const string = o => typeof o === 'string' ? o : literal(o)
const literal = o =>
  o instanceof RegExp ? o.toString() :
  o instanceof Error ? `Error(${o.message})` :
  o instanceof Enum ? o.tag + (o.content === undefined ? '' : `(${literal(o.content)})`) :
  o instanceof Map ? JSON.stringify(Object.fromEntries([...o])) :
  typeof o === 'function' ? o.toString() :
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
const comparable = x =>
  x === undefined ? '' :
  x instanceof Error ? literal(x) :
  x instanceof Enum ? x.tag + comparable(x.content) :
  "'" + JSON.stringify(_comparable(x)) + "'"
const _comparable = x => Array.isArray(x) ? x.map(_comparable) :
  x instanceof Map ? [...x.keys()].sort().map(key => _comparable(x.get(key))) :
  typeof x === 'object' ? Object.keys(x).sort().map(key => _comparable(x[key])) :
  typeof x === 'number' ? (Array(16).join('0') + x).slice(-16) :
  x
const embedded = {
  string,
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
  bool: o => Boolean(o),
  int: s => parseInt(s),
  float: s => parseFloat(s),
  time: (...a) => new Time(...a),
  duration: (...a) => new Duration(...a),
  tuple: (...a) => new Tuple().concat(a),
  ref: ref => ({ref}),
  fn: (...a) => console.dir(a),
  throw: (...a) => { throw new SoftError(a.map(string).join(' ')) },
  __call: f => f(),
  __index: index,
}
const _void = undefined
const reserved = 'num decimal interface implementnum else void any moa test log math rand db use module bytes i8 i16 i32 i64 u8 u16 u32 u64 f16 f32 f64'.split(' ')
const evaluate = (node, obj) => execute(node, Object.fromEntries(Object.entries(obj).map(([key, value]) => [key, {value}])))
const execute = (node, env) => {
  const run = node => Array.isArray(node) ? apply(node) : atom(node)
  const prop = (obj, key) => key in obj ? obj[key] : fail('Missing', key, 'of', Object.keys(obj))
  const lookup = key => key in env ? env[key].value : fail('Missing', key, 'in', Object.keys(env))
  const insert = (key, value) => reserved.includes(key) ? fail('Reserved', key) :
    key in env ? fail('Existed', key) : env[key] = {value}
  const update = (key, value) =>
    Array.isArray(key) && key[0] === '.' && key[1] in env ? env[key[1]].value[key[2]] = value :
    key in env ? env[key].value = value : fail('Missing', key)
  const make_func = (keys, body) => (...values) => execute(body, {...env, ...Object.fromEntries(to_array(keys).map((key, i) => [key, {value: values[i]}]))})
  const def_struct = a => (keys => (...values) => Object.fromEntries(keys.map((key, i) => [key, values[i]])))(a[0] === '__pack' ? a.slice(1).map(([name, type]) => name) : [a[0]])
  const def_enum = a => Object.fromEntries(a.map(x => Array.isArray(x) ?
    [x[0], insert(x[0], v => new Enum(x[0], v))] :
    [x, insert(x, new Enum(x))]))
  const run_switch = (target, a) =>
    a.length === 0 ? fail('Unmatching', target) :
    a[0][0] === 'fn' ? run_capture(target, a[0].slice(1), a.slice(1)) :
    a[0][0] === '_' ? run(a[0][1]) :
    comparable(target) === comparable(run(a[0][0])) ? run(a[0][1]) :
    run_switch(target, a.slice(1))
  const run_capture = (target, cond, remain) =>
    target instanceof Enum && target.tag === cond[0][0] ? execute(cond[1], {...env, ...{[cond[0][1]]: {value: target.content}}}) :
    run_switch(target, remain)
  const iif = a => a.length === 0 ? fail('Invalid iif') :
    a.length === 1 ? run(a[0]) :
    run(a[0]) ? run(a[1]) :
    iif(a.slice(2))
  const pack = ([x, ...xs]) => (x => x instanceof Return ? x : xs.length ? pack(xs) : x)(run(x))
  const rescue = (e, a) =>
    e instanceof SoftError ? (a[0] === 'fn' ? run(a)(e) : run(a)) :
    (() => { throw e })()
  const apply = a =>
    a.length === 1 ? run(a[0]) :
    a[0] === 'if' ? pack([run(a.slice(1, -1)) && run(a.at(-1)), _void]) :
    a[0] === 'iif' ? iif(a.slice(1)) :
    a[0] === 'catch' ? attempt(() => run(a[1]), e => rescue(e, a[2])) :
    a[0] === 'return' ? new Return(run(a.slice(1))) :
    a[0] === 'switch' ? run_switch(run(a[1]), a[2].slice(1)) :
    a[0] === 'fn' ? make_func(a[1], a.slice(2)) :
    a[0] === 'let' ? insert(a[1], run(a.slice(2))) :
    a[0] === 'var' ? insert(a[1], run(a.slice(2))) :
    a[0] === 'def' ? insert(a[1], make_func(a.slice(2, -1), index(a, -1))) :
    a[0] === 'test' ? make_func(a[1], a[2])({eq: (a, b) => comparable(a) === comparable(b) || fail('ne', literal(a), literal(b))}) :
    a[0] === 'struct' ? insert(a[1], def_struct(a[2])) :
    a[0] === 'enum' ? insert(a[1], def_enum(a[2].slice(1))) :
    a[0] === '__pack' ? pack(a.slice(1)) :
    a[0] === '&&' ? run(a[1]) && run(a[2]) :
    a[0] === '||' ? run(a[1]) || run(a[2]) :
    a[0] === ':=' ? update(a[1], run(a.slice(2))) :
    a[0] === '.' ? prop(run(a[1]), a[2]) :
    a[0].toString().match(/^[+\-*/%|&]+=$/) ? update(a[1], apply([a[0].slice(0, -1), ...a.slice(1)])) :
    a.length > 1 ? run(a[0])(...a.slice(1).map(run)) :
    fail('apply', a)
  const to_array = o => Array.isArray(o) ? o : [o]
  const atom = s =>
    typeof s !== 'string' ? s : // already evaluated
    s === 'true' ? true :
    s === 'false' ? false :
    s === 'return' ? new Return(_void) :
    s.startsWith('r"') ? new RegExp(s.slice(2, -1), 'g') :
    s.startsWith("r'") ? new RegExp(s.slice(2, -1), 'g') :
    s.startsWith('"') ? JSON.parse(s.replace(/\n/g, '\\n').replace(/\t/g, '\\t')) :
    (m = s.match(/^(-?[0-9]+)(d|h|m|s|ms|us)/)) ? new Duration(parseFloat(m[1]), m[2]) :
    s.match(/^-?[0-9]/) ? parseFloat(s) :
    s in embedded ? embedded[s] : lookup(s)
  const o = run(node)
  return o instanceof Return ? o.ret : o
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

  // types
  test(false, 'bool("")')
  test(1, 'int("1")')
  test(1.2, 'float("1.2")')
  test("1", 'string(1)')
  test(new Time(2024, 1, 3, 13, 55, 58), 'time(2024 1 3 13 55 58)')
  test(new Duration(3, 'd'), '3d')
  test(new Tuple(1, 2), 'tuple(1 2)')
  test({ref: 1}, 'ref(1)')
  test(1, 'fn(x: x)(1)')

  // containers
  test([], '[]')
  test([1], '[1]')
  test([1,2], '[1 2]')
  test(1, '[1][0]')
  test(2, '[1 2][-1]')
  test({}, 'dict()')
  test({a: 1}, 'dict("a" 1)')

  // branch
  test(1, 'iif true 1 2')
  test(2, 'iif false 1 2')
  test(2, 'iif false 1 true 2 3')
  test(3, 'iif false 1 false 2 3')

  // define
  test(1, 'let a 1\na')
  test(0, 'var a 0\ndef f a:\n  a += 1\nf(a)\na') // local scope
  test(1, 'var a 0\ndef f: a += 1\nf()\na') // outer scope
  test(1, 'def f: 1\nf()')
  test(1, 'def f a: a\nf(1)')
  test('hi', 'struct s:\n  a string\ns("hi").a')
  test(1, 'struct s:\n  a string\n  b int\ns("hi" 1).b')
  test({a: "hi", b: 1}, 'struct s:\n  a string\n  b int\ns("hi" 1)')
  test(new Enum('b'), 'enum a:\n  b\n  c int\nb')
  test(new Enum('c', 1), 'enum a:\n  b\n  c int\nc 1')

  // statement
  test(2, '1\n2')
  test(_void, 'if true: 1')
  test(_void, 'if true: return')
  test(1, 'if true: return 1\nthrow 2')
  test(1, 'switch "a":\n"a": 1\n"b": 2\n_: 3')
  test(2, 'switch "b":\n"a": 1\n"b": 2\n_: 3')
  test(3, 'switch "c":\n"a": 1\n"b": 2\n_: 3')
  test(Error('Unmatching c'), 'switch "c":\n"a": 1\n"b": 2')
  test(1, 'enum a:\n  b\n  c int\nswitch b:\nb: 1\nc n => n')
  test(2, 'enum a:\n  b\n  c int\nswitch c(2):\nb: 1\nc(n) => n')
  //[ ] for while continue break

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
  test(true , 'struct s:\n  a string\n  b int\ns("hi" 1) == s("hi" 1)')
  test(false, 'struct s:\n  a string\n  b int\ns("hi" 1) == s("hi" 2)')
  test(true , 'struct s:\n  a string\n  b int\ns("hi" 1) <= s("hi" 1)')
  test(false, 'struct s:\n  a string\n  b int\ns("hi" 1) < s("hi" 1)')
  test(true , 'struct s:\n  a string\n  b int\ns("hi" 1) < s("hi" 2)')
  test(true , 'struct s:\n  a string\n  b int\ns("hi" 9) < s("hi" 10)')
  test(false, 'struct s:\n  a string\n  b int\ns("hi" 10)< s("hi" 9)')
  test(2, 'struct c:\n  a int\nvar x c 1\nx.a := 2')
  test(2, 'struct c:\n  a int\nvar x c 1\nx.a := 2\nx.a')
  test('ab', '"a" ++ "b"')
  test([1, 2], '[1] ++ [2]')
  test([[1], [2]], '[[1]] ++ [[2]]')
  test({a: 1, b: 3, c: 4}, 'dict("a" 1 "b" 2) ++ dict("b" 3 "c" 4)')

  // runtime error
  test(Error('ZeroDivision'), '1 / 0')
  test(Error('OutOfIndex 0'), '[][0]')
  test(Error('OutOfIndex -2'), '[0][-2]')
  test(Error('a 1'), 'throw "a" 1')
  test('a', 'catch throw("a") e => e.message')
  test(1, 'catch throw("a"): 1')
  test(Error('ZeroDivision'), 'catch 1/0: 1')
  //test(2, 'throw(1) @ 2')
  //test(Error('ZeroDivision'), '1 / 0 @ 2')
  //test(2, '1 / 0 @zdiv 2')
  //test(0, '[][0] @e.ooi => e.index')
  //[ ] catch

  // compile error
  test(Error('Existed z'), 'let z 1\nlet z 2')
  test(Error('Missing z'), 'z := 1')

  // edge case
  test(1, 'var a 0\ndef f:\n  def g: a += 1\n  g()\nf()\na')

  // reserved
  reserved.map(word => test(Error('Reserved ' + word), `let ${word} 1`))

  puts('ok')
}
