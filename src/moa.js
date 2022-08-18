'use strict'

Error.stackTraceLimit = 100

const fs = require('fs')
const put = (...a) => process.stdout.write(...a.map(str))
const puts = (...a) => { console.log(...a.map(str)); return a[a.length - 1] }
const fail = (message, ...a) => { throw Error(`${message}${a.length ? '\n' + str(a) : ''}`) }
const many = (a, f, g) => { while (f()) { a.push(g()) }; return a }

const newToken = (code, pos=0, indent=0, line=0, type=undefined) => ({code, pos, indent, line, type, toString: () => code})
const str = o =>
  Array.isArray(o) ? showNode('(' + o.map(str).join(' ') + ')', o.type) :
  typeof o === 'string' ? o :
  typeof o === 'object' && 'code' in o ? showNode(o.code, o.type) :    // for node
  typeof o === 'object' && o.instance ? str(o.instance) :              // for type
  typeof o === 'object' && 'tid' in o ? o.tid :                        // for type
  typeof o === 'object' && 'targs' in o ? `${o.name}${str(o.targs)}` : // for type
  JSON.stringify(o)
const showNode = (base, type) => base + (type ? `[${type.toString()}]` : '')
const simplifyType = t => (d => t.replace(/\d+/g, s => d[s] ||= Object.keys(d).length + 1))({})
const isOp2 = t => t && t.toString().match(/^[+\-*%/=><|&^]+$/)
const isAssign = t => '+= -= *= /= %= ='.split(' ').includes(t.code)

const tokenize = source => {
  const texts = source.split(/((?: |\n|#[^\n]*)+|[0-9]+(?:\.[0-9]+)?|[+\-*%/=><|&^]+|r?"[^"]*"|`[^`]*`|[A-Za-z0-9_]+(?:,[A-Za-z0-9_]+)*|.)/g).filter(x => x)
  const tokens = []
  let pos = 0
  let line = 1
  let indent = 0
  for (const t of texts) {
    const brs = t.split('\n')
    line += brs.length - 1
    if (brs.length >= 2) {
      indent = brs[brs.length - 1].length
    }
    if (t.replace(/[ \t\n]+/, '')) {
      tokens.push(newToken(t.trim(), pos, indent, line))
    }
    pos += t.length
  }
  return tokens
}

const parse = tokens => {
  let pos = 0
  const to_a = o => Array.isArray(o) ? o : [o]
  const until = (unit, guard) => many([], () => pos < tokens.length && guard(tokens[pos]), () => unit(tokens[pos]))
  const bracket = (unit, end) => (a => { ++pos; return a } )(until(unit, t => t.code !== end))
  const bottom = () => {
    const token = tokens[pos++] || fail('EOT', {tokens})
    switch (token.code) {
      case '{': return [newToken('__dict'), ...bracket(exp, '}')]
      case '(': return bracket(exp, ')')
      case '[': return [newToken('__array'), ...bracket(exp, ']')]
      default: return token
    }
  }
  const exp = () => {
    const lhs = bottom()
    if (pos >= tokens.length) {
      return lhs
    }
    const curr = tokens[pos-1]
    const pred = tokens[pos++]
    const close = curr.pos + curr.code.length === pred.pos
    if (pred.code === '.') {
      const rhs = bottom()
      return isOp2(rhs[0]) || rhs[0] == '__at' ? [rhs[0], [pred, lhs, rhs[1]], rhs[2]] : [pred, lhs, rhs]
    } else if (pred.code === '(' && close) { return [newToken('__call'), lhs, ...bracket(exp, ')')]
    } else if (pred.code === '[' && close) { return [newToken('__at'), lhs, ...bracket(exp, ']')]
    } else if (pred.code === '=>') { return [pred, to_a(lhs), exp()]
    } else if (isOp2(pred)) { return [pred, lhs, exp()]
    } else { --pos; return lhs }
  }
  const unnest = o => Array.isArray(o) && o.length === 1 ? unnest(o[0]) : o
  const consumeLine = () => {
    const head = tokens[pos]
    const ary = until(exp, t => t.line === head.line && t.indent === head.indent && t.code !== ':')
    if (pos < tokens.length && tokens[pos].code === ':') {
      const nt = tokens[++pos]
      if (head.indent === nt.indent) {
        ary.push(unnest(until(exp, t => t.line === nt.line)))
      } else if (head.indent < nt.indent) {
        const nest = until(consumeLine, t => t.indent === nt.indent)
        ary.push(nest.length === 1 ? unnest(nest) : [newToken('__stmt'), ...nest])
      } else {
        fail(`Line is endded with ":" but the following indentations are not expected ${tokens[pos]}`, {tokens})
      }
    }
    return ary.length === 1 ? ary[0] : ary
  }
  return until(consumeLine, t => t.indent === 0)
}

const infer = nodes => {
  let tid = 1
  const prune = t => t.instance ? t.instance = prune(t.instance) : t
  const newVar = () => (o => { o.toString = () => str(o); return o } )({tid: tid++, instance: null})
  const newVars = o => o.map(t => [t.code, newVar()])
  //const isNonGeneric = (name, env) => `___${name}` in env
  //const addNonGenerics = names => names.map((name, i) => [name, i.toString()])
  const func = (a, env) => a.length === 1 ? inf(a[0], env) : body(a, env)
  const body = (a, env) => {
    const args = a.slice(0, -1).map((t, i) => [t.code, newVar()])
    const ret = inf(a[a.length - 1], Object.assign({}, env, Object.fromEntries(args)))
    return [...args.map(x => x[1]), ret]
  }
  const env = {}
  const define = (s, t) => s.split(' ').map(op => env[op] = t.includes(' ') ? t.split(' ') : t)
  define('if', 'bool 0 nil')
  define('case', 'bool 0 0 0')
  define('++', 'string string string')
  define('p', '0 0')
  define('true false', 'bool')
  define('< <= > >=', 'int int bool')
  define('|| &&', 'bool bool bool')
  define('+ - * / % += -= *= /= %=', 'int int int')

  const fresh = o => Array.isArray(o) ? (d => o.map(x => x.match(/^[0-9]/) ? (d[x] ||= newVar()) : x))({}) : o
  const unify = (a, b, node) => {
    a = prune(a)
    b = prune(b)
    return Array.isArray(a) && Array.isArray(b) && a.length === b.length ? a.map((x, i) => unify(x, b[i], node)) :
      a.tid && b.tid && a.tid === b.tid ? a :
      a.tid ? a.instance = b :
      b.tid ? b.instance = a :
      a === b ? a :
      fail(`type miss match between '${str(a)}' and '${str(b)}' around ${str(node)}`, {nodes})
  }
  const call = ([head, ...remains], env) =>
    head == 'fn' ? (a => unify(env[remains[0].code] = newVar(), func(a, env), a))(remains.slice(1)) :
    head == 'return' ? (remains.length === 0 ? 'nil' : inf(remains[0])) :
    head == '__stmt' ? remains.map(x => inf(x, env)).slice(-1)[0] :
    head == '__call' ? call(remains, env) :
    head == '__array' ? fail("not implemented yet 1") :
    head == '__dict' ?  fail("not implemented yet 2") :
    head == '=>' ? func([...remains[0], remains[1]], env) :
    unify(inf(head, env), [...remains.map(x => inf(x, env)), newVar()], [head, ...remains]).slice(-1)[0]
  const inf = (node, env) => node.type ||= _inf(node, env)
  const _inf = (node, env) => Array.isArray(node) ? call(node, env) :
    node.code === 'return' ? 'nil' :
    node.code.match(/^[0-9]+$/) ? 'int' :
    node.code.match(/^["`]/) ? 'string' :
    node.code.match(/^r["`]/) ? 'regexp' :
    fresh(env[node.code] || fail(`not implemented yet ${JSON.stringify(node)}`, {nodes, env}))
  nodes.map(x => inf(x, env, {}))
  return nodes
}

const generate = nodes => {
  const embedded = {
    'array_size': code => `${code}.length`,
    'array_set': code => `((d,a) => a.flatMap(x => x in d ? [] : [d[x]=x]))({}, ${code})`,
    'string_size': code => `${code}.length`,
    'dictionary_keys': code => `Object.keys(${code})`,
    'dictionary_values': code => `Object.values(${code})`,
  }
  const gen = o => Array.isArray(o) ? apply(o) :
    o.code.startsWith('r"') ? `RegExp(${o.slice(1)})` :
    o.code.startsWith('`') ? template(o) :
    o.code
  const template = s => s.replace(/\$[A-Za-z0-9_.]+/g, x => '${' + x.slice(1) + '}')
  const isExp = code => !/^(?:if|for|return)/.test(code)
  const _case = a => a.length === 0 ? (() => {throw Error('Invalid case expression')})() :
    a.length === 1 ? a[0] :
    `${a[0]} ? ${a[1]} : ` + _case(a.slice(2))
  const statement = a => a.map((v, i) => a.length - 1 === i && isExp(v) ? 'return ' + v : v)
  const method = (key, args) => key in embedded ? embedded[key](...args) : `${args[0]}.${args[1]}`
  const struct = (name, fields) => `const ${name} = (${fields}) => ({${fields}})`
  const match = args => {
    const error = '(()=>{throw Error(`Unmatch error: "${__.__tag}"`)})()'
    const g = a => a.length === 3 ? `__.__tag === '${a[0]}' ? (${gen(a[1])} => ${gen(a[2])})(__.__val) : ` :
      a.length === 2 ? `__.__tag === '${a[0]}' ? ${gen(a[1])} : ` :
      (() => {throw Error(`Unexpected condition of match ${a}`)})()
    const match = tail[1].slice(1).map(g).join('\n  ') + error
    return `(__ => ${match})(${args[0]})`
  }
  const adt = o => Array.isArray(o) ?
    `const ${o[0]} = __val => ({__tag:'${o[0]}',__val})` :
    `const ${o} = {__tag:'${o}'}`
  const op2 = (head, lhs, rhs) => {
    switch (head.code) {
      case '=>': return `((${lhs}) => ${rhs})`
      case '/': return `(((l,r) => r === 0 ? (() => { throw Error('Zero division error') })() : l / r)(${lhs},${rhs}))`
      case '++': return `${lhs}.toString() + ${rhs}.toString()`
      default: return isAssign(head) ? `${lhs} ${head} ${rhs}` : `(${lhs} ${head} ${rhs})`
    }
  }
  const apply = node => {
    if (node.length === 0) {
      return 'undefined'
    }
    const [head, ...tail] = node
    const args = tail.map(gen)
    switch (head.code) {
      case '__stmt': return `(() => { ${statement(args)} })()`
      case '__call': return tail[0] == 'fail' ?  `(() => { throw Error(${args[1]}) })()` : args[0] + `(${args.slice(1)})`
      case '__array': return `[${args.join(', ')}]`
      case '__dict': return `({${ tail.map(o => Array.isArray(o) ? `${o[1]}:${o[2]}` : o).join(',') }})`
      case '__at': return `${args[0]}[${args[1]}]`
      case '=>': return `(${(Array.isArray(tail[0]) ? tail[0] : [tail[0].code]).join(", ")}) => ${args[1]}`
      case '.': return method(tail[0].type.name + '_' + args[1], args)
      case 'let': return `const ${args[0]} = ${args[1]}`
      case 'var': return `let ${args[0]} = ${args[1]}`
      case 'fn': return `const ${args[0]} = (${args.slice(1, -1)}) => ${args[args.length - 1]}`
      case 'struct': return struct(tail[0], tail[1].slice(1).map(x => x[0]))
      case 'adt': return tail[1].slice(1).map(adt).join('\n')
      case 'if': return `if (${args[0]}) { ${args[1]} }`
      case 'case': return _case(args)
      case 'match': match(args)
      default: return isOp2(head) ? op2(head, args[0], args[1]) : `${gen(head)}(${args})`
    }
  }
  return nodes.map(gen).join(';\n')
}

const compile = source => {
  if (!source) {
    return {tokens: [], nodes: [], js: ''}
  }
  const tokens = tokenize(source)
  const nodes = infer(parse(tokens))
  const js = generate(nodes)
  return {tokens, nodes, js}
}

const testAll = () => {
  const test = (convert, expect, exp, ...defs) => {
    const source = (defs || []).concat(`fn __main:\n  ${exp.replace(/\n/g, "\n  ")}`).join('\n')
    const {tokens, nodes, js, error} = compile(source)
    let ret
    try {
      ret = eval(`${js}\n__main()`)
    } catch (e) {
      ret = e.message
    }
    const actual = convert(ret, nodes.slice(-1)[0])
    if (actual === expect) {
      put('.')
    } else {
      puts('Failed')
      puts('expect:', expect)
      puts('actual:', actual)
      puts('source:', source)
      puts('js    :', js)
      puts('nodes :', nodes)
      puts('tokens:', tokens)
      puts('error :', error)
      process.exit(1)
    }
  }
  const reject = (exp, ...defs) => test((ret) => /^type miss match/.test(ret) ? ret.slice(0, 15) : ret, 'type miss match', exp, ...defs)
  const inf = (expect, exp, ...defs) => test((_, node) => simplifyType(str(node.type)), expect, exp, ...defs)
  const check = (expect, exp, ...defs) => test((ret) => str(ret), str(expect), exp, ...defs)
  const error = (expect, exp, ...defs) => test((ret) => ret, str(expect), exp, ...defs)
  //inf('bool', 'f 1\nf true', 'fn f a: a')
  //return

  // -- Tests for type inference
  // primitives
  inf('int', '0')
  inf('bool', 'true')
  inf('bool', 'false')
  // exp
  inf('int', '1 + 1')
  inf('bool', '1 < 1')
  // branch
  inf('int', 'case true 1 2')
  inf('bool', 'case true true true')
  inf('nil', 'if true: return')
  inf('int', 'if true: return 1\n2')
  // value
  inf('int', 'fn value: 1')
  // lambda
  inf('int', '() => 1')
  inf('(int int)', 'x => x + 1')
  // simple function
  inf('(int int)', 'fn inc a: a + 1')
  inf('(int int int)', 'fn add a b: a + b')
  // recursion function
  inf('int', 'fn f a: 1 + f(a)\nf 2')
  // generic function
  inf('(1 1)', 'fn f a: a')
  inf('(1 2 1)', 'fn f a b: a')
  inf('(1 2 2)', 'fn f a b: b')
  inf('int', 'f 1', 'fn f a: a')
  //inf('bool', 'f 1\nf true', 'fn f a: a')
  // array
  //inf('array(1)', '[]')

  // -- Tests for executions
  check(1, '1')
  return
/*
  // dict
  inf('dictionary(string int)', '{"a":1}')
  inf('int', '{"a":1}["a"]')
  inf('array(string)', '{"a":1}.keys')
  inf('array(int)', '{"a":1}.values')
  // string
  inf('int', '"hi".size')
  // regexp
  inf('regexp', 'r"hi"')
  inf('bool', 'r"hi".test("h")')
  // generic array
  inf('int', '[].size')
  inf('int', '[1][0]')
  inf('string', '["hi"][0]')
  inf('array(int)', '[1]')
  inf('set(int)', '[1].set')
  // time
  inf('time', 'time(2001 2 3 4 5 6)')
  // combinations
  inf('int', '((f 1) + (g 1))', 'fn f x: x + 1', 'fn g x: x + 2')
  inf('((1 2) (2 3) 1 3)', 'fn _ f g x: g f(x)')
  inf('((1 2 3) (1 2) 1 3)', 'fn _ x y z: x z y(z)')
  inf('(bool (bool bool) (bool bool))', 'fn _ b x: case x(b) x x => b')
  inf('(bool bool)', 'fn _ x: case true x case(x true false)')
  inf('(bool bool bool)', 'fn _ x y: case x x y')
  inf('(1 1)', 'fn _ n: (x => x(y => y))(f => f(n))')
  inf('((1 2) 1 2)', 'fn _ x y: x y')
  inf('((1 2) ((1 2) 1) 2)', 'fn _ x y: x y(x)')
  inf('(1 ((1 2 3) 4 2) (1 2 3) 4 3)', 'fn _ h t f x: f h t(f x)')
  inf('((1 1 2) ((1 1 2) 1) 2)', 'fn _ x y: x y(x) y(x)')
  inf('(((1 1) 2) 2)', 'fn f y: id y(id)', 'fn id x x')
  inf('int', 'fn f: case id(true) id(1) id(2)', 'fn id x x')
  inf('int', 'fn g: f(true) + f(4)', 'fn f x: 3')
  inf('(bool (1 1))', 'fn h b: case b f(g) g(f)', 'fn f x x', 'fn g y y')
  // type errors
  reject('1 + true')
  reject('if true: return 1')
  reject('f()', 'fn f:\n  if true: return 1  \n  "hi"')

  // -- Tests for executions
  // top: node*
  check(3, 'a + b()', 'let a 1\nfn b 2')
  // node: exp+ ("\n" | (":\n" ("  " node)+)?)
  // exp: unit (op2 exp)*
  check(7, '1 + 2 * 3')
  check('hi', '"h" ++ "i"')
  // unit: op1? bottom (prop | call | at)*
  check(false, '!true')
  check(true, '!(true && false)')
  check(1, 'f()[1].size', 'fn f: ["a" "b"]')
  // prop: "." id
  check(2, '"hi".size')
  // call: "(" exp+ ")"
  check(3, 'add(1 2)', 'fn add a b: a + b')
  check(1, 'f()(1)', 'fn f: g\nfn g x: x')
  // at: "[" exp "]"
  check(2, '[1 2][1]')
  check(2, 'a[1]', 'let a [1 2]')
  check(1, 'a[0][0]', 'let a [[1]]')
  // bottom:
  // | "(" exp  ")"                    # priority : 1 * (2 + 3)
  check(9, '(1 + 2) * 3')
  // | "(" exp exp+ ")"                # apply    : f(a) == (f a)
  check(3, '(add 1 2)', 'fn add a b: a + b')
  // | "[" exp* "]"                    # array    : [], [1 2]
  check([], '[]')
  check([1, 2], '[1 2]')
  check([1, 2], '[1 1 2].set')
  check(true, '[1].set[1]')
  check(false, '[1].set[2]')
  // | "{" id* (id ":" exp)* "}"       # dictionary: {}, {one "two":2}
  check({}, '{}')
  check({one: 1, two: 2}, '{one "two":2}', 'let one 1')
  check(['one', 'two'], '{one "two":2}.keys', 'let one 1')
  check([1, 2], '{one "two":2}.values', 'let one 1')
  // | '"' [^"]* '"'                   # string   : "hi"
  check('hi', '"hi"')
  // | '`' ("$" unit | [^"])* '`'      # template : `hi $user.name`
  check('hi moa', '`hi $name`', 'let name "moa"')
  check('a\nb', 'v', 'let v `a\nb`')
  // | '"' [^"]* '"'                   # regexp    : r"hi"
  check(true, 'r"^h".test("hi")')
  check(false, 'r"^h".test("bye")')
  // | id ("," id)* "=>" exp           # lambda   : a,b => a + b
  check(1, 'f(a => 1)', 'fn f g: g()')
  check(3, 'f(a,b => 3)', 'fn f g: g(1 2)')
  // | [0-9]+ ("." [0-9]+)?            # number   : 1, 0.5
  check(1, '1')
  check(0.5, '0.5')
  // | id                              # id       : name
  check(1, 'a', 'let a 1')

  // qw(let var fn struct adt if return case match fail)
  check(1, 'n', 'let n 1')
  check(1, 'var n 0\nn+=1\nn')
  check(1, 'f()', 'fn f: 1')
  check({name: 'apple', price: 199}, 'item("apple" 199)', 'struct item:\n  name string\n  price int')
  check(1, 'if true: return 1\n2')
  check(2, 'if false: return 1\n2')
  check(1, 'case true 1 2')
  check(2, 'case false 1 2')
  check(2, 'case false 1 true 2 3')
  check(3, 'case false 1 false 2 3')
  check(1, 'match a(1):\n  a v: v\n  b v: v.size\n  c: 0', 'adt ab:\n  a int\n  b string\n  c')
  check(2, 'match b("hi"):\n  a v: v\n  b v: v.size\n  c: 0', 'adt ab:\n  a int\n  b string\n  c')
  check(0, 'match c:\n  a v: v\n  b v: v.size\n  c: 0', 'adt ab:\n  a int\n  b string\n  c')
  error('hi', 'fail("hi")')
  error('Zero division error', '1/0')

  // priority of operators
  check(3, '"hi".size + 1')
  check(3, '1 + "hi".size')

  // string methods
  check(1, '"a".size')
  // array methods
  check(1, '[0].size')
  //check([1], '[0].map(n => n + 1)')
*/

  puts('ok')
}


const interactive = async () => {
  puts('Moa 0.0.1 May 23 2022, 21:26')
  const readline = require('node:readline')
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: '> '
  })
  rl.prompt()
  let codes = []
  rl.on('line', (line) => {
    const cmd = line.trim()
    if (['exit', 'quit', 'q'].includes(cmd)) {
      rl.close()
      return
    }
    codes.push(cmd)
    let {js, nodes} = compile(codes.join('\n'))
    js = js.replace(/^let |const /g, 'global.')
    try {
      puts(eval(js))
    } catch (e) {
      puts(e.stack)
    }
    puts('js:', js)
    puts('node:', str(nodes))
    rl.prompt()
  }).on('close', () => {
    puts('Bye👋')
  })
}

const main = () => {
  const { execSync } = require('child_process')
  const [cmd, ...args] = process.argv.slice(2)
  const get = () => compile(args.map(path => fs.readFileSync(path, 'utf-8')).join('\n\n'))
  if (cmd === 'test') {
    testAll()
  } else {
    interactive()
  }
}


main()
