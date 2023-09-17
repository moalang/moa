/*
 * Eval internal expression
 */
class List extends Array { }

const { parse } = require('./parser.js')
const dump = o => { console.dir(o, {depth: null}); return o }
const fail = m => { throw new Error(m) }
const str = o =>
  typeof o === 'string' ? o :
  o instanceof Error ? `${o.constructor.name}(${o.message})` :
  Array.isArray(o) ? `[${o.map(str).join(' ')}]` :
  JSON.stringify(o)
const put = x => process.stdout.write(str(x));
const puts = (...a) => { console.log(...a); return a[0]; }

const isNativeArray = o => Array.isArray(o) && o.constructor === Array
const buildin = {
  new: (run, ...a) => Object.fromEntries(Array(a.length/2).fill().map((_,i) => [a[i*2], run(a[i*2+1])])),
  dict: (run, ...a) => Object.fromEntries(Array(a.length/2).fill().map((_,i) => [run(a[i*2]), run(a[i*2+1])])),
  list: (run, ...a) => new List().concat(a.map(run)),
  tuple: (run, ...a) => new List().concat(a.map(run)),
}
const evaluate = (x, env) => {
  const run = x => evaluate(x, env)
  const lookup = key => env[key] || fail(`Not found '${key}' in [${Object.keys(env)}]`)
  const local = (run, args, argv) => Object.fromEntries(args.map((id, i) => [id, argv[i]]))
  const lambda = (env, args, body) => (caller, argv) => evaluate(body, {...env, ...local(caller, args, argv)})
  const method = (target, id) =>
    id.match(/^[0-9]/) ? run(target)[id] :
    typeof target === 'string' && id === 'size' ? target.length :
    Array.isArray(target) && id === 'size' ? target.length :
    typeof target === 'object' && id in target ? target[id] :
    fail(`'${id}' is unknown method of '${str(target)}'`)
  const op2 = (op, lhs, rhs) => {
    const toComparelable = x => "'" + JSON.stringify(_toComparelable(x)) + "'"
    const _toComparelable = x => Array.isArray(x) ? x.map(_toComparelable) :
      typeof x === 'object' ? Object.keys(x).sort().map(key => _toComparelable(x[key])) :
      typeof x === 'number' ? (Array(16).join('0') + x).slice(-16) :
      x
    return '== != > >= < <='.split(' ').includes(op) ? eval(`${toComparelable(run(lhs))} ${op} ${toComparelable(run(rhs))}`) :
      op === '++' ? run(lhs).concat(run(rhs)) :
      op.match(/^[+\-*/%|&]+=$/) ? env[lhs] = op2(op.slice(0, -1), run(lhs), run(rhs)) :
      eval(`${run(lhs)} ${op} ${run(rhs)}`)
  }
  const struct = xs => (run, ...a) =>
    xs.length === 2 ? {[xs[0]]: run(a[0])} :
    Object.fromEntries(xs.slice(1).map(([id, _], i) => [id, run(a[i])]))
  const unpack = xs => xs[0] === '__pack' ? xs.slice(1) : [xs]
  const union = (id, xs) => unpack(xs).map(x => Array.isArray(x) ? [x[0], (run, v) => ({__tag: x[0], __val: run(v)})] : [x, {__tag: x}])
  const match = (target, conds) => {
    outer: for (const [_, cond, body] of conds) {
      if (Array.isArray(target) && Array.isArray(cond) && cond[0] === 'list') {
        const a = cond.slice(1) // drop 'list' head of the list
        if (a.length === target.length) {
          let capture = {}
          for (var i=0; i<a.length; i++) {
            var x = a[i]
            if (x.startsWith('"')) {
              if (target[i] !== x.slice(1, -1)) {
                continue outer
              }
            } else if (x.match(/^[0-9]+/)) {
              if (target[i] !== parseFloat(x)) {
                continue outer
              }
            } else if (x.match(/^[A-Za-z_]/)) {
              env[x] = target[i]
            } else {
              fail(`${cond} is not supported matching patterns`)
            }
          }
          return evaluate(body, {...env, ...capture})
        }
      } else if (cond[0] === '"') {
        if (target === run(cond)) {
          return run(body)
        }
      } else if (cond[0] === '.') {
        if (cond.length === 2 && target.__tag === cond[1]) {
          return run(body)
        } else if (cond.length === 3 && target.__tag === cond[2]) {
          return evaluate(body, {...env, [cond[1]]: target.__val})
        }
      } else {
        fail(`${conds} are not supported matching patterns`)
      }
    }
    fail(`${conds} are not match with ${str(target)}`)
  }
  const isTuple = t => Array.isArray(t) && t[0] === ','
  const tuple = t => isTuple(t[0]) ? tuple(t[0].slice(1)).concat(run(t[1])) : t.map(run)
  const iif = a => a.length === 1 ? run(a[0]) : run(a[0]) ? run(a[1]) : iif(a.slice(2))
  const eq = (a, b) => ((a, b) => a === b ? undefined : fail(`eq ${str(a)} ${str(b)}`))(str(a), str(b))
  const block = ([head, ...tail], body) =>
    head === 'def' ? env[tail[0]] = lambda(env, tail.slice(1), body) :
    head === 'let' || head === 'var' ? (env[tail[0]] = run(body)) :
    head === 'struct' ? env[tail[0]] = struct(body) :
    head === 'union' ? env[tail[0]] = Object.fromEntries(union(tail[0], body).map(([id, node]) => [id, env[id] = node])) :
    head === 'match' ? match(run(tail[0]), unpack(body)) :
    head === 'test' ? evaluate(body, {...env, [tail[0]]:{eq}}) :
    fail(`'${head}' is unkown block with ${str(tail)} and ${str(body)}`)
  const apply = ([head, ...tail]) =>
    isNativeArray(head) ? apply([run(head), ...tail]) :
    typeof head === 'function' ? head(...tail) :
    head === 'string' ? str(run(tail[0])) :
    head === 'int' ? parseInt(run(tail[0])) :
    head === 'let' || head === 'var' ? (env[tail[0]] = run(...tail.slice(1))) :
    head === 'iif' ? iif(tail) :
    head === '__index' ? run(tail[0])[run(tail[1])] :
    head === '__call' ? lookup(tail[0])(run) :
    head === '__pack' ? tail.map(run).slice(-1)[0] :
    head === '.' ? method(run(tail[0]), tail[1]) :
    head === ':' ? block(tail[0], tail[1]) :
    head === ',' ? tuple(tail) :
    head === '!' ? !run(tail[0]) :
    head.match(/^[+\-*\/%<>|&=!]/) ? op2(head, tail[0], tail[1]) :
    tail.length > 0 ? lookup(head)(run, ...tail) :
    head.startsWith('"') ? head :
    lookup(head)
  return x instanceof List ? x :
    Array.isArray(x) ? apply(x) :
    typeof x === 'number' ? x :
    x === 'true' ? true :
    x === 'false' ? false :
    x.match(/^-?[0-9]/) ? parseFloat(x) :
    x.match(/^["`]/) ? x.slice(1, -1) :
    x.match(/^r"/) ? new RegExp(x.slice(2, -1)) :
    lookup(x)
}

module.exports = { evaluate, buildin }
if (require.main === module) {
  const run = src => {
    try {
      return evaluate(parse(src), buildin)
    } catch (e) {
      return e
    }
  }
  const eq = (expect, src) => {
    const actual = run(src)
    if (str(expect) === str(actual)) {
      put('.')
    } else {
      puts()
      puts(`Expect: ${str(expect)}`)
      puts(`Actual: ${str(actual)}`)
      puts(`Source: ${JSON.stringify(src)}`)
      if (actual instanceof Error) {
        puts('Error: ', actual.stack)
      }
      put('Nodes: ')
      try {
        dump(parse(src))
      } catch (e) {
        throw Error(e, {cause: e})
      }
      throw Error('Test was failed')
    }
  }
  const test = (expect, src) => eq(expect, src)

  // [x] def
  test(1, 'def f a: a\nf(1)')
  test(2, 'def f a:\n  let b a + 1\n  b\nf(1)')
  test(2, 'def f a:\n  var b a\n  a += 1\nf(1)')
  // [x] var
  test(3, 'var a 1\na+=2\na')
  // [x] let
  test(1, 'let a 1\na')
  // [x] struct
  test({}, '{}')
  test({a:1, b:"c"}, '{a=1 b="c"}')
  test('hi', 'struct s:\n  a string\ns("hi").a')
  test(1, 'struct s:\n  a string\n  b int\ns("hi" 1).b')
  test(true, 'struct s:\n  a string\n  b int\ns("hi" 1) == s("hi" 1)')
  test(false, 'struct s:\n  a string\n  b int\ns("hi" 1) == s("hi" 2)')
  test(true, 'struct s:\n  a string\n  b int\ns("hi" 1) <= s("hi" 1)')
  test(false, 'struct s:\n  a string\n  b int\ns("hi" 1) < s("hi" 1)')
  test(true, 'struct s:\n  a string\n  b int\ns("hi" 1) < s("hi" 2)')
  test(true, 'struct s:\n  a string\n  b int\ns("hi" 9) < s("hi" 10)')
  // [x] match
  test(1, 'match "a":\n  "a": 1\n  "b": 2')
  test(1, 'match [1]:\n  [1]: 1\n  [2 n]: n')
  test(3, 'match [2 3]:\n  [1]: 1\n  [2 n]: n')
  test('b', 'match ["a" "b"]:\n  ["a" b]: b')
  // [x] union / match
  test(1, 'union ab:\n  a\n  b\nmatch a:\n  .a: 1\n  .b: 2')
  test(2, 'union ab:\n  a\n  b\nmatch b:\n  .a: 1\n  .b: 2')
  test('hi', 'union ab:\n  a string\n  b int\nmatch a("hi"):\n  s.a: s\n  n.b: string(n)')
  test('1', 'union ab:\n  a string\n  b int\nmatch b(1):\n  s.a: s\n  n.b: string(n)')
  // [x] iif
  test(1, 'iif true 1 2')
  test(2, 'iif false 1 2')
  test(2, 'iif false 1 true 2 3')
  test(3, 'iif false 1 false 2 3')
  // [x] int
  test(1, '1')
  test(-1, '-1')
  test(3, '1 + 2')
  test(1, '3 - 2')
  test(6, '2 * 3')
  test(1.5, '3 / 2')
  test(1, '3 % 2')
  test(27, '3 ** 3')
  test(6, '3 << 1')
  test(3, '6 >> 1')
  test(1, 'int("1")')
  // [x] bool true false
  test(false, '!true')
  test(true, 'true')
  test(false, 'false')
  test(false, 'true && false')
  test(true, 'true && true')
  test(true, 'true || false')
  test(false, '1 < 1')
  test(true, '1 <= 1')
  test(false, '1 > 1')
  test(true, '1 >= 1')
  test(true, '1 == 1')
  test(false, '1 != 1')
  // [x] string
  test('hi', '"hi"')
  test(2, '"hi".size')
  test('h', '"hi"[0]')
  test('1', 'string(1)')
  test('ab', '"a" ++ "b"')
  // [x] tuple
  test([1,2], '1,2')
  test([1,2,3], '1,2,3')
  test([1,2], 'tuple(1 2)')
  test(1, 'tuple(1 2.0).0')
  test(2.0, 'tuple(1 2.0).1')
  // [x] list
  test([], '[]')
  test([1], '[1]')
  test([1,2], '[1 2]')
  test(1, '[1][0]')
  test(0, '[].size')
  test(2, '[1 2].size')
  // [x] dict
  test({}, 'dict()')
  test({s:1}, 'dict("s" 1)')
  test({1:2}, 'dict(1 2)')
  test({s:1}, '[s:1]')
  test({1:2}, '[1:2]')
  test(2, '[1:2][1]')
  // [x] test
  test(undefined, 'test t: t.eq 1 1')
  test(Error('eq 1 2'), 'test t: t.eq 1 2')
  test(Error('eq "1" "2"'), 'test t: t.eq "1" "2"')

  puts('ok')
}
