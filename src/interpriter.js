/*
 * Eval internal expression
 */
class List extends Array { }

const { parse } = require('./parser.js')
const dump = o => { console.dir(o, {depth: null}); return o }
const fail = m => { throw new Error(m) }
const str = o =>
  typeof o === 'string' ? o :
  o instanceof Set ? `set(${[...o].map(str).join(' ')})` :
  Array.isArray(o) ? `[${o.map(str).join(' ')}]` :
  JSON.stringify(o)
const put = x => process.stdout.write(str(x));
const puts = (...a) => { console.log(...a); return a[0]; }

const buildin = {
  new: (run, ...a) => Object.fromEntries(Array(a.length/2).fill().map((_,i) => [a[i*2], run(a[i*2+1])])),
  dict: (run, ...a) => Object.fromEntries(Array(a.length/2).fill().map((_,i) => [run(a[i*2]), run(a[i*2+1])])),
  list: (run, ...a) => new List().concat(a.map(run)),
  set: (run, ...a) => new Set(a.map(run)),
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
  const op2 = (op, lhs, rhs) =>
    '== != > >= < <='.split(' ').includes(op) ? eval(`'${str(lhs)}' ${op} '${str(rhs)}'`) :
    op.match(/^[+\-*/%|&]+=$/) ? env[lhs] = op2(op.slice(0, -1), lhs, rhs) :
    eval(`${lhs} ${op} ${rhs}`)
  const struct = xs => (run, ...a) =>
    xs.length === 2 ? {[xs[0]]: run(a[0])} :
    Object.fromEntries(xs.slice(1).map(([id, _], i) => [id, run(a[i])]))
  const flatten = xs => xs[0] === '__do' ? xs.slice(1).map(flatten) : xs
  const union = (id, xs) => flatten(xs).map(x => Array.isArray(x) ? [x[0], (run, v) => ({__tag: x[0], __val: run(v)})] : [x, {__tag: x}])
  const hit = (target, cond) => cond.match(/^./) ? target.__tag === cond.slice(1) :
    fail(`'${cond}' is unknwon pattern with ${str(target)}`)
  const match = (target, conds) => {
    for (const [_1, _2, cond, body] of conds) {
      if (cond[0] === '.') {
        if (cond.length === 2 && target.__tag === cond[1]) {
          return run(body)
        } else if (cond.length === 3 && target.__tag === cond[2]) {
          return evaluate(body, {...env, ...{[cond[1]]: target.__val}})
        }
      }
    }
    fail(`${conds} are not match with ${str(target)}`)
  }
  const block = (head, ...tail) =>
    head === 'fn' ? env[tail[0][0]] = lambda(env, tail[0].slice(1), tail[1]) :
    head === 'let' || head === 'var' ? (env[tail[0][0]] = run(tail[1])) :
    head === 'struct' ? env[tail[0]] = struct(tail[1]) :
    head === 'union' ? env[tail[0]] = Object.fromEntries(union(tail[0], tail[1]).map(([id, node]) => [id, env[id] = node])) :
    head === 'match' ? match(run(tail[0]), flatten(tail[1])) :
    fail(`'${head}' is unkown block with ${str(tail)}`)
  const apply = ([head, ...tail]) =>
    head === 'string' ? str(run(tail[0])) :
    head === '__call' ? lookup(tail[0])(run) :
    head === '__do' ? tail.map(run).slice(-1)[0] :
    head === '.' ? method(run(tail[0]), tail[1]) :
    head === ':' ? block(...tail) :
    head === '!' ? !run(tail[0]) :
    head === 'let' || head === 'var' ? (env[tail[0]] = run(...tail.slice(1))) :
    head.match(/^[+\-*\/%<>|&=!]/) ? op2(head, run(tail[0]), run(tail[1])) :
    tail.length > 0 ? lookup(head)(run, ...tail) :
    lookup(head)
  return x instanceof List ? x :
    x instanceof Set ? x :
    Array.isArray(x) ? apply(x) :
    typeof x === 'number' ? x :
    x === 'true' ? true :
    x === 'false' ? false :
    x.match(/^[0-9]/) ? parseFloat(x) :
    x.match(/^["`]/) ? x.slice(1, x.length - 1) :
    x.match(/^r"/) ? new RegExp(x.slice(2, x.length - 1)) :
    lookup(x)
}

const input = process.argv[2]
if (input) {
  console.log(str(evaluate(parse(input), buildin)))
} else {
  const test = (expect, src) => {
    expect = str(expect)
    const node = parse(src)
    let fact
    try {
      fact = str(evaluate(node, buildin))
    } catch (e) {
      puts()
      puts(src)
      dump(node)
      throw Error(e, {cause: e})
    }
    if (expect === fact) {
      put('.')
    } else {
      puts(src)
      dump(node)
      throw Error(`'${expect}' != '${fact}'`)
    }
  }
  test('hi', 'union ab:\n  a string\n  b int\nmatch a "hi":\n  case s.a: s\n  case n.b: string(n)')
  test('hi', 'union ab:\n  a string\n  b int\nmatch a "hi":\n  case s.a: s\n  case n.b: string(n)')
  test('1', 'union ab:\n  a string\n  b int\nmatch b 1:\n  case s.a: s\n  case n.b: string(n)')

  // primitives
  test(true, 'true')
  test(false, 'false')
  test(1, '1')
  test(1.2, '1.2')
  test('1', 'string(1)')
  test('hi', '"hi"')
  test('hi', '`hi`')
  test({}, '{}')
  test({a:1, b:"c"}, '{a=1 b="c"}')

  // properties
  test(0, '"".size')
  test(2, '"hi".size')
  test(0, '[].size')
  test(2, '[1 2].size')

  // containers
  test([], '[]')
  test([1], '[1]')
  test([1,2], '[1 2]')
  test(new Set(), 'set()')
  test(new Set([1]), 'set(1)')
  test({}, 'dict()')
  test({s:1}, 'dict("s" 1)')
  test({1:2}, 'dict(1 2)')
  test({s:1}, '[s:1]')
  test({1:2}, '[1:2]')
  test([1,2], 'tuple(1 2)')
  test(1, 'tuple(1 2.0).0')
  test(2.0, 'tuple(1 2.0).1')

  // single operator
  test(false, '!true')

  // binary operators
  test(3, '1 + 2')
  test(1, '3 - 2')
  test(6, '2 * 3')
  test(1.5, '3 / 2')
  test(1, '3 % 2')
  test(27, '3 ** 3')
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

  // user defined algebraic data type
  test(1, 'union ab:\n  a\n  b\nmatch a:\n  case .a: 1\n  case .b: 2')
  test('hi', 'union ab:\n  a string\n  b int\nmatch a "hi":\n  case s.a: s\n  case n.b: string(n)')
  test('hi', 'union ab:\n  a string\n  b int\nmatch a "hi":\n  case s.a: s\n  case n.b: string(n)')
  test('1', 'union ab:\n  a string\n  b int\nmatch b 1:\n  case s.a: s\n  case n.b: string(n)')

  // error handling
  //error('zdiv', '1 / 0')
  //error('zdiv', '1 % 0')
  //error('hi', 'throw "hi"')
  //test('hi', 'try (throw "hi") e => e.message')
  //test('a', 'fn f:\n  if true: throw "a"\n  "b"\nfn g e: e.message\ntry f() g')
  //test('c', 'fn f:\n  if true: throw "a"\n  "b"\nfn g e: "c"\ntry f() g')
  //test('b', 'fn f:\n  if false: throw "a"\n  "b"\nfn g e: "c"\ntry f() g')

  puts('ok')
}
