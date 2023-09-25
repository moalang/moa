/*
 * Eval internal expression
 */
class List extends Array { }
class Tuple extends Array { }
const { parse } = require('./parser.js')
const dump = o => { console.dir(o, {depth: null}); return o }
const fail = m => { throw new Error(m) }
const string = o => typeof o === 'string' ? o : escape(o)
const unescape = s => s.replace(/\\"/g, '"').replace(/\\\\/g, '\\').replace(/\\n/g, '\n').replace(/\\t/g, '\t')
const escape = o =>
  o instanceof RegExp ? o.toString() :
  o instanceof Error ? `Error(${o.message})` :
  o instanceof Tuple ? o.map(escape).join(',') :
  typeof o === 'function' ? o.toString() :
  Array.isArray(o) ? `[${o.map(escape).join(' ')}]` :
  JSON.stringify(o)
const put = x => process.stdout.write(x);
const puts = (...a) => { console.log(...a); return a[0]; }
const tuple = (...a) => new Tuple().concat(...a)
const list = (...a) => new List().concat(...a)
const attempt = (f, g) => { try { return f() } catch (e) { return g(e) } }
const make = a => Object.fromEntries(Array(a.length/2).fill().map((_,i) => [a[i*2], a[i*2+1]]))

const evaluate = (x, env) => {
  const run = x => evaluate(x, env)
  const runWith = (x, ...a) => evaluate(x, {...env, ...make(a)})
  const lookup = key => key in env ? env[key] : fail(`Not found '${key}' in [${Object.keys(env)}]`)
  const lambda = (env, args, body) => (...argv) => runWith(body, ...(args.flatMap((x, i) => [x, argv[i]])))
  const method = (target, id) =>
    target instanceof List && id === 'size' ? target.length :
    target instanceof List && id === 'keep' ? f => target.filter(f) :
    target instanceof List && id === 'slice' ? (...a) => target.slice(...a) :
    target instanceof List && id === 'map' ? (...a) => target.map(...a) :
    target instanceof List && id === 'join' ? (s) => target.join(s) :
    typeof target === 'object' && id in target ? target[id] :
    typeof target === 'string' && id === 'size' ? target.length :
    typeof target === 'string' && id === 'rsplit' ? r => list(...target.split(r)) :
    typeof target === 'string' && id === 'match' ? r => target.match(r) :
    typeof target === 'string' && id === 'starts' ? s => target.startsWith(s) :
    typeof target === 'string' && id === 'split' ? s => list(...target.split(s)) :
    typeof target === 'string' && id === 'slice' ? (...a) => target.slice(...a) :
    typeof target === 'string' && id === 'replace' ? (...a) => target.replace(...a) :
    typeof target === 'string' && id === 'int' ? parseInt(target) :
    typeof target === 'string' && id === 'float' ? parseFloat(target) :
    typeof target === 'string' && id.match(/^[0-9]/) ? index(target)[id] :
    fail(`'${id}' is unknown method of '${typeof target === "object" ? target.constructor : typeof target}'`)
  const op2 = (op, lhs, rhs) => {
    const toComparable = x => "'" + JSON.stringify(_toComparable(x)) + "'"
    const _toComparable = x => Array.isArray(x) ? x.map(_toComparable) :
      typeof x === 'object' ? Object.keys(x).sort().map(key => _toComparable(x[key])) :
      typeof x === 'number' ? (Array(16).join('0') + x).slice(-16) :
      x
    switch (op) {
      case '==': return toComparable(run(lhs)) == toComparable(run(rhs))
      case '!=': return toComparable(run(lhs)) != toComparable(run(rhs))
      case '>=': return toComparable(run(lhs)) >= toComparable(run(rhs))
      case '<=': return toComparable(run(lhs)) <= toComparable(run(rhs))
      case '>' : return toComparable(run(lhs)) > toComparable(run(rhs))
      case '<' : return toComparable(run(lhs)) < toComparable(run(rhs))
      case '++': return run(lhs).concat(run(rhs))
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
      case ':=': return lhs in env ? env[lhs] = run(rhs) : fail(`${lhs} missing`)
      default: return op.match(/^[+\-*/%|&]+=$/) ? env[lhs] = op2(op.slice(0, -1), run(lhs), run(rhs)) : fail(`${op} unknown operator`)
    }
  }
  const unpack = xs => xs[0] === '__pack' ? xs.slice(1) : [xs]
  const union = (id, xs) => unpack(xs).map(x =>
    x[0] === ':' ? [x[1], (...a) => ({__tag: x[1], __val: Object.fromEntries(unpack(x[2]).map(([id, _], i) => [id, a[i]]))})] :
    Array.isArray(x) ? [x[0], __val => ({__tag: x[0], __val})] : [x, {__tag: x}])
  const match = (target, conds) => {
    const rescue = conds.find(a => a[1][0] === '.' && a[1][2] === 'error')
    target = rescue ? attempt(() => run(target), e => ({__tag: 'error', __val: {message: e.message}})) : run(target)
    const _match = ([[_, cond, body], ...left]) =>
      cond[0] === '.' ?  (target.__tag === cond[2] ? runWith(body, cond[1], target.__val) : _match(left)) :
      cond.match(/[A-Za-z_]/) ? runWith(body, cond, target) :
      target === run(cond) && run(body)
    return _match(conds) || fail(`${conds} are not match with ${string(target)}`)
  }
  const iif = a => a.length === 1 ? run(a[0]) : run(a[0]) ? run(a[1]) : iif(a.slice(2))
  const eq = (a, b) => ((a, b) => a === b ? undefined : fail(`eq ${a} ${b}`))(escape(a), escape(b))
  const assign = (name, value) => name in env ? fail(`${name} exists`) : env[name] = value
  const index = (a, i) => i < 0 ? index(a, a.length + i) : i >= a.length ? fail(`${i} exceeded`) : a[i]
  const define = ([head, body]) => Array.isArray(head) ?
    assign(head[0], lambda(env, head.slice(1), body))  :
    assign(head, run(body))
  const block = ([head, ...tail], body) =>
    head === 'class' ? env[tail[0]] = (...a) => Object.fromEntries(unpack(body).map(([id, _], i) => [id, a[i]])) :
    head === 'union' ? env[tail[0]] = Object.fromEntries(union(tail[0], body).map(([id, node]) => [id, env[id] = node])) :
    head === 'match' ? match(tail[0], unpack(body)) :
    head === 'test' ? runWith(body, tail[0], {eq}) :
    fail(`'${head}' is unkown block with ${string(tail)} and ${string(body)}`)
  const apply = ([head, ...tail]) =>
    head === undefined ? undefined :
    Array.isArray(head) ? apply([run(head), ...tail]) :
    typeof head === 'function' ? head(...tail.map(run)) :
    head === 'list' ? list(...tail.map(run)) :
    head === 'tuple' ? tuple(...tail.map(run)) :
    head === 'class' ? Object.fromEntries(Array(tail.length/2).fill().map((_,i) => [tail[i*2], run(tail[i*2+1])])) :
    head === 'dict' ? Object.fromEntries(Array(tail.length/2).fill().map((_,i) => [run(tail[i*2]), run(tail[i*2+1])])) :
    head === 'error' ? fail(string(run(tail[0]))) :
    head === 'string' ? escape(run(tail[0])) :
    head === 'iif' ? iif(tail) :
    head === '=' ? define(tail) :
    head === '__index' ? index(run(tail[0]), run(tail[1])) :
    head === '__call' && tail[0] === 'class' ? ({}) :
    head === '__call' && tail[0] === 'dict' ? ({}) :
    head === '__call' && tail[0] === 'list' ? list() :
    head === '__call' ? lookup(tail[0])(run) :
    head === '__pack' ? tail.map(run).slice(-1)[0] :
    head === '.' ? method(run(tail[0]), tail[1]) :
    head === ':' && tail[0] === 'iif' ? iif(unpack(tail[1]).flatMap(a => a.slice(1))) :
    head === ':' ? block(tail[0], tail[1]) :
    head === ',' ? tuple(...tail.map(run)) :
    head === '!' ? !run(tail[0]) :
    head === '=>' ? lambda(env, Array.isArray(tail[0]) ? tail[0].filter(x => x !== ',') : [tail[0]], tail[1]) :
    head.match(/^[+\-*\/%<>|&=!:]/) ? op2(head, tail[0], tail[1]) :
    tail.length > 0 ? lookup(head)(...tail.map(run)) :
    lookup(head)
  return x instanceof List ? x :
    x instanceof Tuple ? x :
    Array.isArray(x) ? apply(x) :
    typeof x !== 'string' ? x :
    x === '_' ? true : // for pattern match and iif
    x === 'true' ? true :
    x === 'false' ? false :
    x.startsWith('r"') ? new RegExp(x.slice(2, -1), "g") :
    x.startsWith('"') ? unescape(x.slice(1, -1)) :
    x.match(/^-?[0-9]/) ? parseFloat(x) :
    lookup(x)
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
      dump(parse(src))
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

  // string
  test('hi', '"hi"')
  test('\t', '"\\t"')
  test('hi', '"""hi"""')
  test('\t', '"""\\t"""')
  test('h"i', '"""h"i"""')
  test(/\t/g, 'r"\\t"')
  test(/\t/g, 'r"""\\t"""')
  test(2, '"hi".size')
  test(['', '1', '', '2', ''], '"12".rsplit(r"([0-9])")')
  test('h', '"hi"[0]')
  test('ab', '"a" ++ "b"')
  test(1, '"1".int')
  test(1.1, '"1.1".float')
  test(['a', 'b'], '"a b".split(" ")')
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
  test([1], '[1]')
  test([1,2], '[1 2]')
  test(1, '[1][0]')
  test(2, '[1 2][-1]')
  test(Error('0 exceeded'), '[][0]')
  test(0, '[].size')
  test(2, '[1 2].size')
  test([2], '[1 2].keep x => x > 1')

  // dict
  test({}, 'dict()')
  test({s:1}, 'dict("s" 1)')
  test({1:2}, 'dict(1 2)')
  test({s:1}, '[s:1]')
  test({1:2}, '[1:2]')
  test(2, '[1:2][1]')

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
  test(Error('z missing'), 'z := 1')

  // define
  test(1, 'a = 1\na')
  test(1, 'f = () => 1\nf()')
  test(1, 'f a = a\nf(1)')
  test(2, 'f a =\n  b = a + 1\n  b\nf(1)')
  test(1, 'a = 1\nf a =\n  a += 1\nf(a)\na')
  test(Error('z exists'), 'z = 1\nz = 2')

  // class
  test({}, '{}')
  test({a:1, b:"c"}, '{a=1 b="c"}')
  test('hi', 'class s:\n  a string\ns("hi").a')
  test(1, 'class s:\n  a string\n  b int\ns("hi" 1).b')
  test(true, 'class s:\n  a string\n  b int\ns("hi" 1) == s("hi" 1)')
  test(false, 'class s:\n  a string\n  b int\ns("hi" 1) == s("hi" 2)')
  test(true, 'class s:\n  a string\n  b int\ns("hi" 1) <= s("hi" 1)')
  test(false, 'class s:\n  a string\n  b int\ns("hi" 1) < s("hi" 1)')
  test(true, 'class s:\n  a string\n  b int\ns("hi" 1) < s("hi" 2)')
  test(true, 'class s:\n  a string\n  b int\ns("hi" 9) < s("hi" 10)')

  // union / match
  test(1, 'union u:\n  a\nmatch a:\n  .a: 1')
  test(2, 'union u:\n  a n\nmatch a(2):\n  n.a: n')
  test(3, 'union u:\n  a:\n    b int\nmatch a(3):\n  o.a: o.b')
  test(9, 'union u:\n  a:\n    b int\n    c int\nmatch a(4 5):\n  o.a: o.b + o.c')

  // error / match
  test(Error('1'), 'error 1')
  test("f", 'f s = error "f"\nmatch f("t"):\n  e.error: e.message\n  s: s')
  test("t", 'f s = s\nmatch f("t"):\n  e.error: e.message\n  s: s')

  // iif
  test(1, 'iif true 1 2')
  test(2, 'iif false 1 2')
  test(2, 'iif false 1 true 2 3')
  test(3, 'iif false 1 false 2 3')
  test(1, 'iif:\n  true: 1\n  _: 2')
  test(2, 'iif:\n  false: 1\n  _: 2')

  // test
  test(undefined, 'test t: t.eq 1 1')
  test(Error('eq 1 2'), 'test t: t.eq 1 2')
  test(Error('eq "a" "b"'), 'test t: t.eq "a" "b"')

  // edge case
  test(undefined, '')
  test(undefined, '\n')

  puts('ok')
}
