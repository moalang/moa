// Eval internal expression

const { parse } = require('./parser.js')
class List extends Array {
  get size() { return this.length }
}
class Tuple extends Array { }
class SoftError extends Error {
  constructor(a) {
    super(a.join(' '))
    this.id = a[0]
  }
}
class Return { constructor(ret) { this.ret = ret } }
class Continue { }
class Break { }
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
const fail = (...a) => { throw new Error(a.filter(x => x !== undefined).map(string).join(' ')) }
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
  '++': (l, r) =>
    l instanceof Map ? new Map([...l, ...r]) :
    l instanceof Set ? new Set([...l, ...r]) :
    l.concat(r),
  list: (...a) => new List().concat(a),
  dict: (...a) => a.length === 1 ?
    new Map(a[0]) :
    new Map(range(a.length / 2).map(i => [a[i*2], a[i*2+1]])),
  set: (...a) => new Set(a),
  bool: o => Boolean(o),
  int: s => parseInt(s),
  float: s => parseFloat(s),
  time: (...a) => new Time(...a),
  duration: (...a) => new Duration(...a),
  tuple: (...a) => new Tuple().concat(a),
  ref: ref => ({ref}),
  throw: (...a) => { throw new SoftError(a.map(string)) },
  continue: new Continue(),
  break: new Break(),
  __call: f => f(),
  __index: index,
}
const methods = {
  string: {
    size: s => s.length,
    trim: s => () => s.trim(),
    split: s => t => new List().concat(s.split(t)),
    has: s => t => s.includes(t),
    match: s => r => Boolean(s.match(r)),
    replace: s => (r, f) => s.replace(r, f),
  },
  list: {
    a: a => a.length,
    has: a => x => a.includes(x),
    map: a => f => a.map(x => f(x)),
    mapi: a => f => a.map((x, i) => f(x, i)),
    map0: a => f => a.map(x => (x[0] = f(x[0]), x)),
    map1: a => f => a.map(x => (x[1] = f(x[1]), x)),
    filter: a => f => a.filter(x => f(x)),
    slice: a => (n, m) => a.slice(n, m),
    join: a => s => a.map(string).join(s),
    push: a => x => a.push(x),
  },
  dict: {
    get: d => k => d.has(k) ? d.get(k) : fail('KeyNotFound', k),
    set: d => (k,v) => (d.set(k, v), v),
    has: d => k => d.has(k),
    find: d => f => (x => x ? new Tuple().concat(x) : fail('NotFound', d))([...d.entries()].find(([k,v]) => f(k, v))),
    //vmap: d => f => { for (const [k, v] of d) { d.set(k, f(v)); }; return d },
  },
  set: {
    set: s => k => (b => (s.set(k), b))(s.has(k)),
    has: s => k => s.has(k),
  },
  number: {
    string: n => n.toString(),
  },
  boolean: {
    int: b => Number(b),
  }
}
const fs = require('node:fs')
const { execSync } = require('child_process')
const tester = ({
  eq: (a, b) => comparable(a) === comparable(b) || (fail('ne', literal(a), literal(b))),
  go: (expect, src) => {
    const go = "package main\n" + src + "\n"
    fs.writeFileSync('/tmp/moa.go', go, {encoding: 'utf-8'})
    const actual = execSync('command go run /tmp/moa.go', {encoding: 'utf-8'})
    return expect === actual || fail('ne', expect, actual)
  },
})
const _void = undefined
const reserved = 'num decimal interface implementnum else void any moa test log math rand db use module bytes i8 i16 i32 i64 u8 u16 u32 u64 f16 f32 f64'.split(' ')
const evaluate = (node, obj) => execute(node, Object.fromEntries(Object.entries(obj).map(([key, value]) => [key, {value}])))
const execute = (node, env) => {
  const bind = (obj, target) => typeof target === 'function' ? target.bind(obj) : target
  const run = node => Array.isArray(node) ? apply(node) : atom(node)
  const prop = (obj, key) =>
    obj instanceof List && key in methods.list ? methods.list[key](obj) :
    obj instanceof Map && key in methods.dict ? methods.dict[key](obj) :
    typeof obj === 'object' && key in obj ? bind(obj, obj[key]) :
    typeof obj !== 'object' && key in methods[typeof obj] ? methods[typeof obj][key](obj) :
    fail('Property', key, 'of', typeof obj === 'object' ? Object.keys(obj) : typeof obj)
  const lookup = key => key in env ? env[key].value : fail('Missing', key, 'in', Object.keys(env))
  const insert = (key, value) => reserved.includes(key) ? fail('Reserved', key) :
    key in env ? fail('Existed', key) : (env[key] = {value}, value)
  const update = (key, value) =>
    Array.isArray(key) && key[0] === '.' && key[1] in env ? env[key[1]].value[key[2]] = value :
    key in env ? env[key].value = value : fail('Missing', key)
  const make_func = (keys, body) => (...values) => execute(body, {...env, ...Object.fromEntries(to_array(keys).map((key, i) => [key, {value: values[i]}]))})
  const def_struct = keys => (...values) => Object.fromEntries(keys.map(([key], i) => [key, values[i]]))
  const def_enum = a => Object.fromEntries(a.map(([name, type]) =>
    [name, insert(name, enum_content(name, type))]))
  const enum_content = (name, type) =>
    !type ? new Enum(name) :
    Array.isArray(type) ? (...a) => new Enum(name, Object.fromEntries(unpack(type).map(([t,_], i) => [t, a[i]]))) :
    v => new Enum(name, v)
  const run_while = (cond, body) => run(cond) && !(execute(body, {...env}) instanceof Break) ? run_while(cond, body) : _void
  const run_case = (target, a) =>
    a.length === 0 ? fail('Unmatching', target) :
    a[0][0].startsWith('r"') && target.match(run(a[0][0])) ? run(a[0][1]) :
    a[0][0] === '=>' ? run_capture(target, a[0].slice(1), a.slice(1)) :
    a[0][0] === '_' ? run(a[0][1]) :
    comparable(target) === comparable(run(a[0][0])) && case_if(a[0].slice(1, -1)) ? run(a[0].at(-1)) :
    run_case(target, a.slice(1))
  const case_if = a => a[0] !== 'if' || run(a.slice(1))
  const run_capture = (target, [cond, body], remain) =>
    cond[0] === '.' && target.tag === cond[2] ? execute(body, {...env, ...{[cond[1]]: {value: target.content}}}) :
    cond.length === 2 && target.tag === cond[0] ? execute(body, {...env, ...{[cond[1]]: {value: target.content}}}) :
    run_case(target, remain)
  const iif = a =>
    a.length === 1 && a[0][0] === '__pack' ? iif_remain(a[0].slice(1, -1).flatMap(x => x).concat([a[0].at(-1)])) :
    a.length === 2 ? (run(a[0]) ? run(a[1][1]) : run(a[1][2])) :
    iif_remain(a)
  const iif_remain = a => a.length === 0 ? fail('Undetermined') :
    a.length === 1 ? run(a[0]) :
    run(a[0]) ? run(a[1]) :
    iif_remain(a.slice(2))
  const unpack = a => a[0] == '__pack' ? a.slice(1) : [a]
  const pack = ([x, ...xs]) => (x =>
    x instanceof Return ? x :
    x instanceof Continue ? x :
    x instanceof Break ? x :
    xs.length ? pack(xs) : x)(run(x))
  const rescue = (e, a) =>
    e instanceof SoftError ? (a[0] === '=>' ? run(a)(e) : run(a)) :
    (() => { throw e })()
  const call = a => (([f, ...args]) => typeof f === 'function' ? f(...args) : fail('not a function', f, a))(a.map(run))
  const apply = a =>
    a.length === 0 ? fail('empty apply', node) :
    a.length === 1 ? run(a[0]) :
    a[0] === 'if' ? run(a.slice(1, -1)) && run(a.at(-1)) :
    a[0] === 'iif' ? iif(a.slice(1)) :
    a[0] === 'catch' ? attempt(() => run(a[1]), e => rescue(e, a[2])) :
    a[0] === 'return' ? new Return(run(a.slice(1))) :
    a[0] === 'case' ? run_case(run(a.slice(1, -1)), unpack(a.at(-1))) :
    a[0] === 'while' ? run_while(a.slice(1, -1), a.at(-1)) :
    a[0] === '=>' ? make_func(a[1], a.slice(2)) :
    a[0] === 'let' ? insert(a[1], run(a.slice(2))) :
    a[0] === 'var' ? insert(a[1], run(a.slice(2))) :
    a[0] === 'def' ? insert(a[1], make_func(a.slice(2, -1), index(a, -1))) :
    a[0] === 'test' ? make_func(a[1], a.at(-1))(tester) :
    a[0] === 'struct' ? insert(a[1], def_struct(unpack(a[2]))) :
    a[0] === 'enum' ? insert(a[1], def_enum(unpack(a[2]))) :
    a[0] === '__pack' ? pack(a.slice(1)) :
    a[0] === '&&' ? run(a[1]) && run(a[2]) :
    a[0] === '||' ? run(a[1]) || run(a[2]) :
    a[0] === '=' ? update(a[1], run(a.slice(2))) :
    a[0] === '.' ? prop(run(a[1]), a[2]) :
    a[0].toString().match(/^[+\-*/%|&]+=$/) ? update(a[1], apply([a[0].slice(0, -1), ...a.slice(1)])) :
    a.length >= 2 ? call(a) :
    fail('apply', a)
  const to_array = o => Array.isArray(o) ? o : [o]
  const unquote = c => c === 'n' ? '\n' :
    c == 't' ? '\t' :
    c
  const atom = s =>
    typeof s !== 'string' ? s : // already evaluated
    s === 'true' ? true :
    s === 'false' ? false :
    s === 'return' ? new Return(_void) :
    s.startsWith('r"') ? new RegExp(s.slice(2, -1), 'g') :
    s.startsWith("r'") ? new RegExp(s.slice(2, -1), 'g') :
    s.startsWith('"') ? s.slice(1, -1).replace(/\\./g, s => unquote(s[1])) :
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
    const e = comparable(expect)
    const a = comparable(actual)
    if (e === a) {
      put('.')
    } else {
      puts()
      puts(`Expect: ${e}`)
      puts(`Actual: ${a}`)
      puts(`Source: ${JSON.stringify(src)}`)
      put('Nodes : ')
      log(parse(src))
      if (actual instanceof Error) {
        puts(actual.stack)
      }
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
  test('h"i', "\"h\\\"i\"")
  test('\\', '"\\\\"')
  test('\t', '"\\t"')
  test('\\t', '"\\\\t"')
  test('\\\t', '"\\\\\\t"')
  test(/[a-z]\t/g, 'r"[a-z]\\t"')
  test(1, '(a => a)(1)')
  test(3, '(a,b => a + b)(1 2)')
  test(6, '(a,b,c => a + b + c)(1 2 3)')
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

  // containers
  test([], '[]')
  test([1], '[1]')
  test([1,2], '[1 2]')
  test(1, '[1][0]')
  test(2, '[1 2][-1]')
  test({}, 'dict()')
  test({a: 1}, 'dict("a" 1)')
  test({a: 1}, 'dict([tuple("a" 1)])')
  test(1, 'dict("a" 1).find(k,v => k == "a").1')
  //test({a: 2}, 'dict("a" 1).vmap(v => v + 1)')
  test(new Error('KeyNotFound a'), 'dict().get("a")')
  test(new Set(), 'set()')

  // branch
  test(1, 'iif true 1 2')
  test(2, 'iif false 1 2')
  test(2, 'iif false 1 true 2 3')
  test(3, 'iif false 1 false 2 3')
  test(3, 'iif:\n  false: 1\n  false: 2\n  3')
  test(1, 'iif true:\n  1\n  2')
  test(2, 'iif false:\n  1\n  2')
  test(1, 'iif 1 < 2:\n  1\n  2')
  test(5, 'iif:\n  false: 1\n  2 + 3')

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
  test(1, 'if true: return 1\n2')
  test(2, 'if false: return 1\n2')
  test(1, 'def f n: n\nif true: return f 1')
  test(1, 'if true: return 1\nthrow 2')
  test(2, 'if false: return 1\n2')
  test(1, 'case "a":\n  "a": 1\n  "b": 2\n  _: 3')
  test(2, 'case "b":\n  "a": 1\n  "b": 2\n  _: 3')
  test(3, 'case "c":\n  "a": 1\n  "b": 2\n  _: 3')
  test(2, 'case "a":\n  "a" if false: 1\n  "a" if 1 == 1: 2')
  test(1, 'case "a":\n  r"a": 1\n  "a": 2')
  test(Error('Unmatching c'), 'case "c":\n  "a": 1\n  "b": 2')
  test(1, 'enum a:\n  b\n  c int\ncase b:\n  b: 1\n  c n => n')
  test(2, 'enum a:\n  b\n  c int\ncase c(2):\n  b: 1\n  c(n) => n')
  test(2, 'enum a:\n  b\n  c int\ncase c 2:\n  b: 1\n  c(n) => n')
  test(3, 'enum a:\n  b:\n    v int\n    x int\ncase b 1 2:\n  o.b => o.v + o.x')
  test(1, 'enum a:\n  b c d\ncase b 1: b(n) => n')
  test(3, 'let n 1\nwhile n < 3: n += 1\nn')
  test(1, 'let n 1\nwhile true:\n  if true: break\n  n+=1\nn')
  test(3, 'let n 1\nwhile n < 3:\n  n += 1\n  if true: continue\n  n+=5\nn')
  test(7, 'let n 1\nwhile n < 3:\n  n += 1\n  if false: continue\n  n+=5\nn')
  test(3, 'let n 1\nwhile n < 3:\n  let m 1\n  n += m\nn')

  // test
  test(true, 'test t: t.eq 1 1')
  test(Error('ne 1 2'), 'test t: t.eq 1 2')
  test(Error('ne "a" "b"'), 'test t: t.eq "a" "b"')
  test(true, 'test t: t.go "go" "import \\"fmt\\"\\nfunc main() { fmt.Printf(\\"go\\") }"')

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
  test(2, 'var a 1\na = 2\na')
  test(true , 'struct s:\n  a string\n  b int\ns("hi" 1) == s("hi" 1)')
  test(false, 'struct s:\n  a string\n  b int\ns("hi" 1) == s("hi" 2)')
  test(true , 'struct s:\n  a string\n  b int\ns("hi" 1) <= s("hi" 1)')
  test(false, 'struct s:\n  a string\n  b int\ns("hi" 1) < s("hi" 1)')
  test(true , 'struct s:\n  a string\n  b int\ns("hi" 1) < s("hi" 2)')
  test(true , 'struct s:\n  a string\n  b int\ns("hi" 9) < s("hi" 10)')
  test(false, 'struct s:\n  a string\n  b int\ns("hi" 10)< s("hi" 9)')
  test(2, 'struct c:\n  a int\nvar x c 1\nx.a = 2')
  test(2, 'struct c:\n  a int\nvar x c 1\nx.a = 2\nx.a')
  test('ab', '"a" ++ "b"')
  test([1, 2], '[1] ++ [2]')
  test([[1], [2]], '[[1]] ++ [[2]]')
  test({a: 1, b: 3, c: 4}, 'dict("a" 1 "b" 2) ++ dict("b" 3 "c" 4)')
  test(new Set([1, 2]), 'set(1) ++ set(2)')

  // runtime error
  test(Error('ZeroDivision'), '1 / 0')
  test(Error('OutOfIndex 0'), '[][0]')
  test(Error('OutOfIndex -2'), '[0][-2]')
  test(Error('a 1'), 'throw "a" 1')
  test('a', 'catch throw("a") e => e.message')
  test(1, 'catch throw("a"): 1')
  test(Error('ZeroDivision'), 'catch 1/0: 1')

  // compile error
  test(Error('Existed z'), 'let z 1\nlet z 2')
  test(Error('Missing z'), 'z = 1')

  // reserved
  reserved.map(word => test(Error('Reserved ' + word), `let ${word} 1`))

  // methods
  test('a', '" a ".trim()')
  test(['a', 'b'], '"a b".split(" ")')
  test(true, '"a".has "a"')
  test(true, '[1].has 1')
  test(0, '"".size')
  test(true, '"a".match(r"a")')
  test([2], '[1].map x => x + 1')
  test([1, 2], '[1 1].mapi x,i => x + i')
  test([new Tuple().concat([0, 3])], '[tuple(1 3)].map0 x => x - 1')
  test([new Tuple().concat([1, 2])], '[tuple(1 3)].map1 x => x - 1')
  test([1], '[1 2].filter x => x == 1')
  test(0, '[].size')
  test([2], '[1 2].slice 1')
  test([2], '[1 2 3].slice 1 (-1)')
  test('1 2', '[1 2].join " "')
  test(2, '[1 2].at 1')
  test(1, 'true.int')
  test(0, 'false.int')
  test('1', '1.string')

  // edge case
  test(1, 'var a 0\ndef f:\n  def g: a += 1\n  g()\nf()\na')
  test(1, 'let a case 0:\n  0: 1\n  1: 2')
  test(1, 'def f a b: a\nif true:\n  f "c" 1\n1')

  puts('ok')
}
