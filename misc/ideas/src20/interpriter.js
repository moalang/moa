/*
 * Eval internal expression
 */
class List extends Array { }
class Tuple extends Array { }
const none = { and: _ => none, or: v => v }
const some = x => ({ and: f => some(f(x)), or: _ => x, __value: x })
function Return(value) { this.value = value }
Return.prototype.valueOf = function() { return this.value }
const { parse } = require('./parser.js')
const log = o => { console.dir(o, {depth: null}); return o }
const fail = m => { throw new Error(m) }
const string = o => typeof o === 'string' ? o : escape(o)
const escape = o =>
  o instanceof RegExp ? o.toString() :
  o instanceof Error ? `Error(${o.message})` :
  o instanceof Tuple ? o.map(escape).join(',') :
  typeof o === 'function' ? o.toString() :
  typeof o === 'object' && o.constructor === Map ? JSON.stringify(Object.fromEntries([...o])) :
  Array.isArray(o) ? `[${o.map(escape).join(' ')}]` :
  JSON.stringify(o)
const put = x => process.stdout.write(x);
const puts = (...a) => { console.log(...a); return a[0]; }
const range = (n, m) => [...Array(n)].map((_, i) => i + (m || 0))
const tuple = (...a) => new Tuple().concat(...a)
const list = (...a) => new List().concat(...a.map(x => Array.isArray(x) ? [x] : x))
const set = (...a) => new Set(a)
const dict = (...a) => new Map(range(a.length / 2).map(i => [a[i*2], a[i*2+1]]))
const attempt = (f, g) => { try { return f() } catch (e) { return g(e) } }
const make_env = o => Object.fromEntries(Object.keys(o).map(key => [key, {value: o[key]}]))
const evaluate = (x, env) => execute(x, make_env({...env}))
const execute = (x, env) => {
  const run = x => execute(x, env)
  const run_with = (x, e) => execute(x, {...env, ...make_env(e)})
  const lookup = key => key in env ? env[key].value : fail(`Not found '${key}' in [${Object.keys(env)}]`)
  const declare = (name, value) => name in env ? fail(`${name} exists`) : (env[name] = {value}, value)
  const update = (key, value) =>
    key.includes('.') ? key.slice(1).reduce((e, name) => name in e.value ? e.value[name] : fail(`${name} not in ${string(e)}`), {value: env}).value = value :
    key in env ? env[key].value = value : fail(`${key} missing`)
  const lambda = (args, body) => (...argv) => run_with(body, Object.fromEntries(args.map((x, i) => [x, argv[i]])))
  const method = (target, id) =>
    target instanceof List && id === 'size' ? target.length :
    target instanceof List && id === 'keep' ? f => target.filter(f) :
    target instanceof List && id === 'slice' ? (...a) => target.slice(...a) :
    target instanceof List && id === 'map' ? (...a) => target.map(...a) :
    target instanceof List && id === 'fmap' ? (...a) => target.flatMap(...a) :
    target instanceof List && id === 'join' ? (s) => target.join(s) :
    target instanceof List && id === 'has' ? (s) => target.includes(s) :
    target instanceof List && id === 'bool' ? target.length >= 1 :
    target instanceof List && id === 'find' ? (f => Boolean(target.find(f))) :
    target instanceof List && id === 'index' ? (s => target.findIndex(x => op2('==', x, s))) :
    target instanceof List && id === 'dict' ? () => new Map([...target.map(x => [...x])]) :
    target instanceof List && id === 'zip' ? (a => range(Math.min(target.length, a.length)).map(i => tuple(target[i], a[i]))) :
    target instanceof List && id === 'all' ? (f => target.find(x => !f(x)) === undefined) :
    target instanceof List && id === 'size' ? target.length :
    target instanceof Map && id === 'vmap' ? f => new Map([...target.entries()].map(([k,v]) => [k, f(v)])) :
    target instanceof Map && id === 'has' ? x => target.has(x) :
    target instanceof Map && id === 'get' ? x => target.get(x) :
    target instanceof Set && id === 'add' ? x => target.add(x) :
    typeof target === 'object' && id in target ? target[id] :
    typeof target === 'string' && id === 'size' ? target.length :
    typeof target === 'string' && id === 'rsplit' ? r => list(...target.split(r)) :
    typeof target === 'string' && id === 'match' ? r => Boolean(target.match(r)) :
    typeof target === 'string' && id === 'starts' ? s => target.startsWith(s) :
    typeof target === 'string' && id === 'split' ? s => list(...target.split(s)) :
    typeof target === 'string' && id === 'slice' ? (...a) => target.slice(...a) :
    typeof target === 'string' && id === 'replace' ? (...a) => target.replace(...a) :
    typeof target === 'string' && id === 'int' ? parseInt(target) :
    typeof target === 'string' && id === 'float' ? parseFloat(target) :
    typeof target === 'string' && id === 'has' ? (s => target.includes(s)) :
    typeof target === 'string' && id.match(/^[0-9]/) ? index(target)[id] :
    typeof target === 'number' && id === 'string' ? target.toString() :
    typeof target === 'boolean' && id === 'int' ? (target ? 1 : 0) :
    fail(`'${id}' is unknown method of '${typeof target === "object" ? target.constructor : typeof target}'`)
  const op2 = (op, lhs, rhs) => {
    const to_comparable = x => "'" + JSON.stringify(_to_comparable(x)) + "'"
    const _to_comparable = x => Array.isArray(x) ? x.map(_to_comparable) :
      typeof x === 'object' ? Object.keys(x).sort().map(key => _to_comparable(x[key])) :
      typeof x === 'number' ? (Array(16).join('0') + x).slice(-16) :
      x
    const concat = (l, r) =>
      l.concat ? l.concat(r) :
      l.constructor === Set ? new Set([...l, ...r]) :
      new Map([...l, ...r])
    switch (op) {
      case '==': return to_comparable(run(lhs)) == to_comparable(run(rhs))
      case '===': return run(lhs).__tag === rhs
      case '!=': return to_comparable(run(lhs)) != to_comparable(run(rhs))
      case '>=': return to_comparable(run(lhs)) >= to_comparable(run(rhs))
      case '<=': return to_comparable(run(lhs)) <= to_comparable(run(rhs))
      case '>' : return to_comparable(run(lhs)) > to_comparable(run(rhs))
      case '<' : return to_comparable(run(lhs)) < to_comparable(run(rhs))
      case '++': return concat(run(lhs), run(rhs))
      case '+' : return run(lhs) + run(rhs)
      case '-' : return run(lhs) - run(rhs)
      case '*' : return run(lhs) * run(rhs)
      case '/' : return (r => r === 0 ? fail('zero division') : run(lhs) / r)(run(rhs))
      case '%' : return run(lhs) % run(rhs)
      case '**': return run(lhs) ** run(rhs)
      case '>>': return run(lhs) >> run(rhs)
      case '<<': return run(lhs) << run(rhs)
      case '&&': return run(lhs) && run(rhs)
      case '||': return run(lhs) || run(rhs)
      case ':=': return lhs[0] === '__index' ? set_env(run(lhs[1]), run(lhs[2]), run(rhs)) :
          lhs[0] === '.' ? run(lhs[1])[lhs[2]] = run(rhs) :
          update(lhs, run(rhs))
      default: return op.match(/^[+\-*/%|&]+=$/) ? update(lhs, op2(op.slice(0, -1), run(lhs), run(rhs))) : fail(`${op} unknown operator`)
    }
  }
  const unpack = xs => xs[0] === '__pack' ? xs.slice(1) : [xs]
  const union = (id, xs) => unpack(xs).map(x =>
    x[0] === ':' ? [x[1], (...a) => ({__tag: x[1], __val: Object.fromEntries(unpack(x[2]).map(([id, _], i) => [id, a[i]]))})] :
    Array.isArray(x) ? [x[0], __val => ({__tag: x[0], __val})] : [x, {__tag: x}])
  const match = (target, conds) => {
    const rescue = conds.find(a => a[1][0] === '.' && a[1][2] === 'error')
    target = rescue ? attempt(() => run(target), e => ({__tag: 'error', __val: {message: e.message}})) : run(target)
    const branch = left => {
      const f = ([[_, cond, body], ...left]) =>
        cond[0] === '.' ?  (target.__tag === cond[2] ? run_with(body, {[cond[1]]: target.__val}) : branch(left)) :
        cond.match(/[A-Za-z_]/) ? run_with(body, {[cond]: target}) :
        target === run(cond) ? run(body) :
        branch(left)
      return left.length === 0 ? fail(`not match ${string(target)} with:\n- ${conds.map(string).join("\n- ")}`) : f(left)
    }
    return branch(conds)
  }
  const capture = (cond, e) => Array.isArray(cond) && cond[0] === '&&' ? capture(cond[1], e) && capture(cond[2], e) :
    run_with(cond, e) && ((Array.isArray(cond) && cond[0] === '===' && cond[1].match(/^[A-Za-z_]/) && (e[cond[1]]=lookup(cond[1]).__val, true)) || true)
  const guard = (cond, body) => (e => capture(cond, e) ? new Return(run_with(body, e)) : true)({})
  const eq = (a, b, c) => ((a, b) => a === b ? true : fail(`${a} ne ${b}` + (c ? ` # ${JSON.stringify(c)}` : '')))(escape(a), escape(b))
  const to_key = o => typeof o === 'object' ? JSON.stringify(o) : o
  const set_env = (a, k, v) => Array.isArray(a) ? a[to_key(k)] = v : (a.set(to_key(k), v), v)
  const index = (a, i) =>
    Array.isArray(a) || typeof a === 'string' ? (i < 0 ? index(a, a.length + i) : i >= a.length ? fail(`${i} exceeded`) : a[i]) :
    a.has(to_key(i)) ? a.get(to_key(i)): fail(`${to_key(i)} not found in ${JSON.stringify(a)}`)
  const define = ([head, body]) => Array.isArray(head) ?
    declare(head[0], lambda(head.slice(1), body))  :
    declare(head, run(body))
  const block = ([head, ...tail], body) =>
    head === 'class' ? declare(tail[0], (...a) => Object.fromEntries(unpack(body).map(([id, _], i) => [id, a[i]]))) :
    head === 'union' ? declare(tail[0], Object.fromEntries(union(tail[0], body).map(([id, node]) => (declare(id, node), [id, node])))) :
    head === 'match' ? match(tail[0], unpack(body)) :
    head === 'test' ? run_with(body, {[tail[0]]: {eq}}) :
    head === 'guard' ? guard(tail[0], body) :
    head === 'def' ? declare(tail[0], lambda(tail.slice(1), body)) :
    fail(`'${head}' is unkown block with ${string(tail)} and ${string(body)}`)
  const apply = ([head, ...tail]) =>
    head === undefined ? undefined :
    Array.isArray(head) ? apply([run(head), ...tail]) :
    typeof head === 'function' ? head(...tail.map(run)) :
    head === 'list' ? list(...tail.map(run)) :
    head === 'set' ? set(...tail.map(run)) :
    head === 'tuple' ? tuple(...tail.map(run)) :
    head === 'dict' ? dict(...tail.map(run)) :
    head === 'class' ? Object.fromEntries(Array(tail.length/2).fill().map((_,i) => [tail[i*2], run(tail[i*2+1])])) :
    head === 'error' ? fail(tail.map(run).map(string).join(' ')) :
    head === 'string' ? escape(run(tail[0])) :
    head === 'guard' ? guard(tail[0], tail[1]) :
    head === '=' ? tail[0][0] == '.' ? run(tail[0][1])[tail[0][2]] = run(tail[1]) : declare(tail[0], run(tail[1])) :
    head === '__index' ? index(run(tail[0]), run(tail[1])) :
    head === '__call' && tail[0] === 'class' ? ({}) :
    head === '__call' && tail[0] === 'list' ? list() :
    head === '__call' && tail[0] === 'set' ? set() :
    head === '__call' && tail[0] === 'dict' ? dict() :
    head === '__call' ? run(tail[0])(run) :
    head === '__pack' ? tail.reduce((prev, x) => prev instanceof Return ? prev : run(x), null).valueOf() :
    head === '.' ? method(run(tail[0]), tail[1]) :
    head === ':' ? block(tail[0], tail[1]) :
    head === ',' ? tuple(...tail.map(run)) :
    head === '!' ? !run(tail[0]) :
    head === '=>' ? lambda(Array.isArray(tail[0]) ? tail[0].filter(x => x !== ',') : [tail[0]], tail[1]) :
    head === 'some' ? some(...tail.map(run)) :
    head.match(/^[+\-*\/%<>|&=!:]/) ? op2(head, tail[0], tail[1]) :
    tail.length > 0 ? lookup(head)(...tail.map(run)) :
    lookup(head)
  const unescape = s => JSON.parse(s.replace(/\n/g, '\\n').replace(/\t/g, '\\t'))
  const ret = x instanceof List ? x :
    x instanceof Tuple ? x :
    Array.isArray(x) ? apply(x) :
    typeof x !== 'string' ? x :
    x === 'true' ? true :
    x === 'false' ? false :
    x === 'some' ? some :
    x === 'none' ? none :
    x.startsWith('r"') ? new RegExp(x.slice(2, -1), 'g') :
    x.startsWith('"') ? unescape(x) :
    x.match(/^-?[0-9]/) ? parseFloat(x) :
    x.match(/^[A-Za-z_]/) ? lookup(x) : x
  if (ret === undefined) {
    fail(`${x} is undefined`)
  }
  return ret
}

module.exports = { evaluate }
if (require.main === module) {
  const eq = (expect, src) => {
    const actual = attempt(() => evaluate(parse(src), {}), e => e)
    if (string(expect) === string(actual)) {
      put('.')
    } else {
      puts()
      puts(`Expect: ${string(expect)}`)
      puts(`Actual: ${string(actual)}`)
      puts(`Source: ${JSON.stringify(src)}`)
      if (actual instanceof Error) {
        puts('Error: ', actual.stack)
      }
      put('Nodes: ')
      log(parse(src))
      throw Error('Test was failed')
    }
  }
  const test = (expect, src) => eq(expect, src)

  // int
  test(1, '1')
  test(-1, '-1')

  // bool true false
  test(true, 'true')
  test(false, 'false')
  test(1, 'true.int')
  test(0, 'false.int')

  // string
  test('hi', '"hi"')
  test('h"i', '"h\\"i"')
  test('\\', '"\\\\"')
  test('\t', '"\\t"')
  test('\\t', '"\\\\t"')
  test('\\\t', '"\\\\\\t"')
  test(/[a-z]\t/g, 'r"[a-z]\\t"')
  test(2, '"hi".size')
  test(['', '1', '', '2', ''], '"12".rsplit(r"([0-9])")')
  test('h', '"hi"[0]')
  test('ab', '"a" ++ "b"')
  test(1, '"1".int')
  test(1.1, '"1.1".float')
  test(['a', 'b'], '"a b".split(" ")')
  test('"\\t"', 'string("\\t")')
  test('"\\\\t"', 'string("\\\\t")')
  test('"\\\\\\t"', 'string("\\\\\\t")')
  '1 -1 0.1 true "s" [] 0,1'.split(' ').map(s => test(s, `string(${s})`))

  // lambda
  test(1, '(a => a)(1)')
  test(3, '(a,b => a + b)(1 2)')
  test('s', '(a => a)("s")')

  // tuple
  test(tuple(1, 2), '1,2')
  test(tuple(1, 2, 3), '1,2,3')
  test(tuple(1, 2), 'tuple(1 2)')
  test(1, 'tuple(1 2.0).0')
  test(2.0, 'tuple(1 2.0).1')

  // list
  test([], '[]')
  test(0, '[].size')
  test([1], '[1]')
  test([1,2], '[1 2]')
  test(1, '[1][0]')
  test(2, '[1 2][-1]')
  test(Error('0 exceeded'), '[][0]')
  test(0, '[].size')
  test(2, '[1 2].size')
  test([2], '[1 2].keep x => x > 1')
  test(0, '[1].index 1')
  test(-1, '[1].index 2')
  test("a", '["a"].zip(["1"])[0].0')
  test(1, '["a"].zip(["1"])[0].1')
  test({a: 1}, '["a",1].dict()')
  test(true, '[].all(x => x == 1)')
  test(true, '[1].all(x => x == 1)')
  test(false, '[2].all(x => x == 1)')

  // set
  test(new Set(), 'set()')
  test(new Set([1, 2]), 'set(1) ++ set(2)')

  // dict
  test({}, 'dict()')
  test({a: 1}, 'dict("a" 1)')
  test({a: 1, b: 2}, 'dict("a" 1) ++ (dict("b" 2))')
  test(1, 'dict("a" 1)["a"]')
  test({a:1}, 'd=dict()\nd["a"]:=1\nd')
  test({a:2}, 'dict("a" 1).vmap(n => n + 1)')

  // option
  test(none, 'none')
  test(some(1), 'some 1')
  test(true, 'none == none')
  test(true, 'some(1) == some(1)')
  test(false, 'some(1) == some(2)')
  test(false, 'some(1) == none')
  test(none, 'none.and(a => a)')
  test(1, 'none.or(1)')
  test(some(2), 'some(1).and(a => a + 1)')
  test(0, 'some(0).or(1)')

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
  test(Error('zero division'), '3 / 0')
  test(1, '3 % 2')
  test(27, '3 ** 3')
  test(6, '3 << 1')
  test(3, '6 >> 1')
  test(3, 'a = 1\na += 2\na')
  test(2, 'a = 1\na := 2\na')
  test(2, 'class c:\n  a int\nx = c(1)\nx.a = 2')
  test(2, 'class c:\n  a int\nx = c(1)\nx.a = 2\nx.a')
  test(2, 'class c:\n  a int\nx = c(1)\nx.a := 2')
  test(2, 'class c:\n  a int\nx = c(1)\nx.a := 2\nx.a')
  test(Error('z missing'), 'z := 1')

  // define
  test(1, 'def f: 1\nf()')
  test(1, 'def f a: a\nf(1)')
  test(0, 'a = 0\ndef f a:\n  a += 1\nf(a)\na') // local scope
  test(1, 'a = 0\ndef f: a += 1\nf()\na') // outer scope
  test(2, 'def f a:\n  m = a % 2\n  guard m == 0 a\n  f(a - 1)\nf(3)') // recursion

  // statement
  test(2, 'guard false: 1\n2')
  test(1, 'guard true: 1\n2')
  test(1, 'guard 1==1:\n  1\n2')
  test(2, 'guard 1==2:\n  1\n2')
  test(Error('z exists'), 'z = 1\nz = 2')

  // match
  test(1, 'match 1:\n  1: 1')
  test(2, 'match 2:\n  1: 1\n  2: 2')
  test(1, 'match 1:\n  _: 1')

  // class
  test('hi', 'class s:\n  a string\ns("hi").a')
  test(1, 'class s:\n  a string\n  b int\ns("hi" 1).b')
  test(true, 'class s:\n  a string\n  b int\ns("hi" 1) == s("hi" 1)')
  test(false, 'class s:\n  a string\n  b int\ns("hi" 1) == s("hi" 2)')
  test(true, 'class s:\n  a string\n  b int\ns("hi" 1) <= s("hi" 1)')
  test(false, 'class s:\n  a string\n  b int\ns("hi" 1) < s("hi" 1)')
  test(true, 'class s:\n  a string\n  b int\ns("hi" 1) < s("hi" 2)')
  test(true, 'class s:\n  a string\n  b int\ns("hi" 9) < s("hi" 10)')

  // union / guard / martch
  test(true, 'union u:\n  a\na === a')
  test(false, 'union u:\n  a\n\n  b\na === b')
  test(1, 'union u:\n  a\nmatch a:\n  _.a: 1')
  test(2, 'union u:\n  a n\nmatch a(2):\n  n.a: n')
  test(3, 'union u:\n  a:\n    b int\nmatch a(3):\n  o.a: o.b')
  test(9, 'union u:\n  a:\n    b int\n    c int\nmatch a(4 5):\n  o.a: o.b + o.c')
  test(1, 'union u:\n  a int\nv = a(1)\nguard v === a: v')
  test(1, 'union u:\n  a int\nv = a(1)\nguard v === a && true: v')
  test(1, 'union u:\n  a int\nv = a(1)\nguard v === a && v == 1: v')

  // error / match
  test(Error('1'), 'error 1')
  test(Error('1 2'), 'error 1 2')
  test("f", 'def f s: error "f"\nmatch f("t"):\n  e.error: e.message\n  s: s')
  test("t", 'def f s: s\nmatch f("t"):\n  e.error: e.message\n  s: s')

  // test
  test(true, 'test t: t.eq 1 1')
  test(Error('1 ne 2'), 'test t: t.eq 1 2')
  test(Error('"a" ne "b"'), 'test t: t.eq "a" "b"')

  // edge case
  test(1, 'a = 0\ndef f:\n  def g: a += 1\n  g()\nf()\na')

  puts('ok')
}
