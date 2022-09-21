'use strict'

Error.stackTraceLimit = 100

const fs = require('fs')
const put = (...a) => { process.stdout.write(...a.map(str)); return a[a.length - 1] }
const puts = (...a) => { console.log(...a.map(str)); return a[a.length - 1] }
const fail = (message, ...a) => { throw Error(`${message}${a.length ? '\n' + str(a) : ''}`) }
const many = (a, f, g) => { while (f()) { a.push(g()) }; return a }

const newToken = (code, pos=0, indent=0, line=0, type=undefined) => ({code, pos, indent, line, type, toString: () => code})
const str = o =>
  Array.isArray(o) ? showNode('(' + o.map(str).join(' ') + ')', o.type) :
  typeof o === 'string' ? o :
  typeof o === 'object' && 'code' in o ? showNode(o.code, o.type) :          // for node
  typeof o === 'object' && o.instance ? str(o.instance) :                    // for type
  typeof o === 'object' && 'generics' in o ? `${o.name}${str(o.generics)}` : // for type
  typeof o === 'object' && 'tid' in o ? o.tid.toString() :                   // for type
  JSON.stringify(o)
const showNode = (base, type) => base + (type ? `<${str(type)}>` : '')
const simplifyType = t => (d => t.replace(/\d+/g, s => d[s] ||= Object.keys(d).length + 1))({})
const isOp1 = t => t && t.toString() === '!'
const isOp2 = t => t && t.toString().match(/^[+\-*%/=><|&^]+$/)
const isAssign = t => '+= -= *= /= %= ='.split(' ').includes(t.code)

const stdlib = (() => {
const ___str = o => typeof o === 'function' && o.length === 0 ? ___str(o()) : o
}).toString().slice(8, -1)

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
  const exp = () => right(bottom())
  const right = lhs => {
    if (pos >= tokens.length) { return lhs }
    const curr = tokens[pos-1]
    const pred = tokens[pos++]
    const close = curr.pos + curr.code.length === pred.pos
    if (pred.code === '.') {
      const rhs = bottom()
      return right(isOp2(rhs[0]) || rhs[0] == '__at' ? [rhs[0], [pred, lhs, rhs[1]], rhs[2]] : [pred, lhs, rhs])
    } else if (pred.code === '(' && close) { return right([newToken('__call'), lhs, ...bracket(exp, ')')])
    } else if (pred.code === '[' && close) { return right([newToken('__at'), lhs, ...bracket(exp, ']')])
    } else if (pred.code === '=>') { return right([pred, to_a(lhs), exp()])
    } else if (isOp1(pred)) { return [lhs, exp()]
    } else if (isOp2(pred)) { return right([pred, lhs, exp()])
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
  const method = (type, name, args) => {
    if (str(type) === 'string' && name === 'size') { return 'int' }
    if (str(type) === 'regexp' && name === 'test') { return ['string', 'bool'] }
    if (type.name === 'array' && name === '__at') { return type.generics[0] }
    if (type.name === 'dict' && name === '__at') { return type.generics[1] }
    if (type.name === 'dict' && name === 'keys') { return newType('array', type.generics.slice(0, 1)) }
    if (type.name === 'dict' && name === 'values') { return newType('array', type.generics.slice(1)) }
    fail(`${str(type)}.${name} can not be inferred`)
  }
  let tid = 1
  const newVar = () => (o => { o.toString = () => str(o); return o } )({tid: tid++, instance: null})
  const newVars = o => o.map(t => [t.code, newVar()])
  const toVars = a => (d => a.map(x => x.toString().match(/^[0-9]/) ? (d[x] ||= newVar()) : x))({})
  const newType = (name, generics) => ({
    name,
    generics,
    isGeneric: generics.length > 0,
    copy: () => ({name, generics: toVars(generics)})
  })

  const prune = t => Array.isArray(t) ? (t.length === 1 ? prune(t[0]) : t.map(prune)) :
    t.instance ? t.instance = prune(t.instance) :
    t
  const env = {
    __array: newType('array', [newVar()]),
    __dict: newType('dict', [newVar(), newVar()]),
  }
  const define = (s, t) => s.split(' ').map(op => env[op] = t.includes(' ') ? toVars(t.split(' ')) : t)
  define('if', 'bool 0 nil')
  define('case', 'bool 0 0 0')
  define('++', 'string string string')
  define('p', '0 0')
  define('true false', 'bool')
  define('< <= > >=', 'int int bool')
  define('!', 'bool bool')
  define('|| &&', 'bool bool bool')
  define('+ - * / % += -= *= /= %=', 'int int int')

  const func = (a, env) => a.length === 1 ? stmt(a, env) : lambda(a, env)
  const lambda = (a, env) => {
    const args = a.slice(0, -1).map((t, i) => [t.code, newVar()])
    const ret = inf(a[a.length - 1], Object.assign({}, env, Object.fromEntries(args)))
    return [...args.map(x => x[1]), ret]
  }
  const copy = o => o.isGeneric ? _copy(o, {}) : o
  const _copy = (o, ids) => Array.isArray(o) ? o.map(x => _copy(x, ids)) : __copy(prune(o), ids)
  const __copy = (o, ids) => o.isGeneric ? o.copy() : o.tid ? ids[o.tid] ||= newVar() : o
  const lookup = (env, node) => env[node.code] || fail(`not found ${JSON.stringify(node)}`, Object.keys(env))
  const unify = (a, b, node) => {
    a = prune(a)
    b = prune(b)
    return Array.isArray(a) && Array.isArray(b) && a.length === b.length ? a.map((x, i) => unify(x, b[i], node)) :
      a.tid && b.tid && a.tid === b.tid ? a :
      a.tid ? a.instance = b :
      b.tid ? b.instance = a :
      str(a) === str(b) ? a :
      fail(`type miss match between '${str(a)}' and '${str(b)}' around ${str(node)}`, str(nodes))
  }
  const stmt = (nodes, env) => {
    for (const node of nodes) {
      if (Array.isArray(node) && node[0].code === 'fn') {
        env[node[1].code] = Object.assign(node.slice(1, -1).map(_ => newVar()), {isGeneric: true, isFunc: true})
      }
    }
    nodes.map(node => inf(node, env)).slice(-1)[0]
    return nodes.map(node => inf(node, env)).slice(-1)[0]
  }
  const call = ([head, ...remains], env) =>
    head == 'fn' ? (exp => unify(lookup(env, remains[0]), exp, remains))(func(remains.slice(1), env)) :
    head == 'let' ? (env[remains[0].code] = inf(remains[1], env)) :
    head == 'return' ? (remains.length === 0 ? 'nil' : inf(remains[0], env)) :
    head == '__stmt' ? stmt(remains, env) :
    head == '__call' ? (remains.length === 1 ? call(remains, env)[0] : call(remains, env)) :
    head == '__array' ? (t => { remains.map(x => unify(inf(x, env), t.generics[0])); return t })(lookup(env, head)) :
    head == '__dict' ?  (t => { remains.map((x, i) => i % 2 == 0 ? unify(t.generics[0], inf(x, env)) : unify(t.generics[1], inf(x, env))) ; return t })(lookup(env, head)) :
    head == '__at' ?  method(inf(remains[0], env), '__at', remains[1]) :
    head == '=>' ? func([...remains[0], remains[1]], env) :
    head == '.' ? method(inf(remains[0], env), remains[1].code, ...remains.slice(2)) :
    remains.length === 0 ? inf(head, env) :
    unify(inf(head, env), [...remains.map(x => inf(x, env)), newVar()], [head, ...remains]).slice(-1)[0]
  const inf = (node, env) => node.type ||= _inf(node, env)
  const _inf = (node, env) => Array.isArray(node) ? call(node, env) :
    node.code === 'return' ? 'nil' :
    node.code.match(/^[0-9]+$/) ? 'int' :
    node.code.match(/^["`]/) ? 'string' :
    node.code.match(/^r["`]/) ? 'regexp' :
    copy(lookup(env, node))
  stmt(nodes, env)
  return nodes
}

const generate = nodes => {
  const embedded = {
    'array_size': code => `${code}.length`,
    'array_set': code => `((d,a) => a.flatMap(x => x in d ? )] : [d[x]=x]))({}, ${code})`,
    'string_size': code => `${code}.length`,
    'dictionary_keys': code => `Object.keys(${code})`,
    'dictionary_values': code => `Object.values(${code})`,
  }
  const gen = o => Array.isArray(o) ? apply(o) :
    o.code === '__array' ? '[]' :
    o.code === '__dict' ? '({})' :
    o.code.startsWith('r"') ? `RegExp(${o.code.slice(1)})` :
    o.code.startsWith('`') ? template(o.code) :
    o.code
  const template = s => s.replace(/\$[A-Za-z0-9_.]+/g, x => '${___str(' + x.slice(1) + ')}')
  const isExp = code => !/^(?:if|for|return)/.test(code)
  const _case = a => a.length === 0 ? (() => {throw Error('Invalid case expression')})() :
    a.length === 1 ? a[0] :
    `${a[0]} ? ${a[1]} : ` + _case(a.slice(2))
  const stmt = a => a.map((v, i) => a.length - 1 === i && isExp(v) ? 'return ' + v : v).join('\n')
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
  const op1 = (head, lhs) => head.code + '(' + lhs + ')'
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
      case '__stmt': return `(() => { ${stmt(args)} })()`
      case '__call': return tail[0] == 'fail' ?  `(() => { throw Error(${args[1]}) })()` : args[0] + `(${args.slice(1)})`
      case '__array': return `[${args.join(', ')}]`
      case '__dict': return `({${args.map((x, i) => x + (i%2 === 0 ? ":" : ",")).join(" ")}})`
      case '__at': return `${args[0]}[${args[1]}]`
      case '=>': return `((${(Array.isArray(tail[0]) ? tail[0] : [tail[0].code]).join(", ")}) => ${args[1]})`
      case '.': return method(tail[0].type + '_' + args[1], args)
      case 'let': return `const ${args[0]} = ${args[1]}`
      case 'var': return `let ${args[0]} = ${args[1]}`
      case 'fn': return `const ${args[0]} = (${args.slice(1, -1)}) => ${args[args.length - 1]}`
      case 'struct': return struct(tail[0], tail[1].slice(1).map(x => x[0]))
      case 'adt': return tail[1].slice(1).map(adt).join('\n')
      case 'if': return `if (${args[0]}) { ${args[1]} }`
      case 'case': return _case(args)
      case 'match': match(args)
      default:
        if (isOp1(head)) {
          return op1(head, args[0])
        } else if (isOp2(head)) {
          return op2(head, args[0], args[1])
        } else {
          return args.length === 0 ? gen(head) : `${gen(head)}(${args})`
        }
    }
  }
  return stdlib + nodes.map(gen).join(';\n')
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
  const inf = (expect, exp, ...defs) => test((_, node) => simplifyType(str(node.type)), expect, exp, ...defs)
  const eq = (expect, exp, ...defs) => test((ret) => str(ret), str(expect), exp, ...defs)
  const error = (expect, exp, ...defs) => test((ret) => ret, str(expect), exp, ...defs)

  // -- Tests for type inference
  // primitives
  inf('int', '0')
  inf('bool', 'true')
  inf('bool', 'false')
  inf('string', '"hi"')
  inf('regexp', 'r"hi"')
  // generic array
  inf('array(1)', '[]')
  inf('array(int)', '[1]')
  inf('array(string)', '[""]')
  inf('int', '[1][0]')
  inf('string', '[""][0]')
  inf('string', '[""][0]')
  // generic dictionary
  inf('dict(1 2)', '{}')
  inf('dict(string int)', '{"a" 2}')
  inf('int', '{"a" 2}["a"]')
  inf('array(string)', '{"a" 2}.keys')
  inf('array(int)', '{"a" 2}.values')
  // methods
  inf('int', '"hi".size')
  inf('bool', 'r"hi".test("h")')
  // exp
  inf('int', '1 + 1')
  inf('bool', '1 < 1')
  inf('bool', 'true && r"hi".test("h")')
  // branch
  inf('int', 'case true 1 2')
  inf('bool', 'case true true true')
  inf('nil', 'if true: return')
  inf('int', 'if true: return 1\n2')
  // variable
  inf('int', 'let value 1')
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
  inf('bool', 'f 1\nf true', 'fn f a: a')
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
  inf('(((1 1) 2) 2)', 'fn f y: id y(id)', 'fn id x: x')
  inf('int', 'fn f: case id(true) id(1) id(2)', 'fn id x x')
  inf('int', 'fn g: f(true) + f(4)', 'fn f x: 3')
  inf('(bool (1 1))', 'fn h b: case b f(g) g(f)', 'fn f x x', 'fn g y y')

  // -- Tests for executions
  // top: line*
  eq(3, 'a + b()', 'let a 1\nfn b 2')
  // line: keyword? exp+ ("\n" | (":\n" ("  " line)+))
  // exp: unit (op2 exp)*
  eq(3, '1 + 2')
  eq(7, '1 + 2 * 3')
  eq('hi', '"h" ++ "i"')
  // unit: op1? bottom (prop | call | at)*
  eq(false, '!true')
  eq(true, '!(true && false)')
  eq(1, 'f()[1].size', 'fn f: ["a" "b"]')
  // prop: "." id
  eq(2, '"hi".size')
  // call: "(" exp+ ")"
  eq(1, 'f()', 'fn f: 1')
  // at: "[" exp "]"
  eq(1, '[1 2][0]')
  eq(2, '[1 2][1]')
  // bottom:
  // | "(" exp ")"                  # priority  : 1 * (2 + 3)
  eq(3, '(1 + 2)')
  eq(9, '(1 + 2) * 3')
  // | "[" exp* "]"                 # array     : [], [1 2]
  eq([], '[]')
  eq([1], '[1]')
  eq([1, 2], '[1 2]')
  // | "{" (exp exp)* "}"           # dictionary: {}, {a "a" (1 + 2) "b"}
  eq({}, '{}')
  eq({a: 1}, '{"a" 1}')
  eq({a: 1, b: 2}, '{"a" 1 "b" 2}')
  // | '"' [^"]* '"'                # string    : "hi"
  eq('hi', '"hi"')
  // | '`' ("$" unit | [^"])* '`'   # template  : `hi $user.name`
  eq('hi moa 5', 'let age 5\n`hi $name $age`', 'fn name: "moa"')
  // | '"' [^"]* '"'                # regexp    : r"hi"
  eq(true, 'r"h".test("hi")')
  // | id ("," id)* "=>" exp        # lambda    : a,b => a + b
  eq(1, '(x => x)(1)')
  // | [0-9]+ ("." [0-9]+)?         # number    : 1, 0.5
  eq(1, '1')
  // | id                           # id        : name
  eq(1, 'id', 'let id: 1')
  eq(1, 'id()', 'fn id: 1')
  // id: [A-Za-z_][A-Za-z0-9_]*
  // op1: "!"
  // op2: [+-/%*=<>|&^;]+
  // keyword: qw(let var fn struct adt if return case match fail test)

/*
  eq(1, 'f()[1].size', 'fn f: ["a" "b"]')
  // prop: "." id
  eq(2, '"hi".size')
  // call: "(" exp+ ")"
  eq(3, 'add(1 2)', 'fn add a b: a + b')
  eq(1, 'f()(1)', 'fn f: g\nfn g x: x')
  // at: "[" exp "]"
  eq(2, '[1 2][1]')
  eq(2, 'a[1]', 'let a [1 2]')
  eq(1, 'a[0][0]', 'let a [[1]]')
  // bottom:
  // | "(" exp  ")"                    # priority : 1 * (2 + 3)
  eq(9, '(1 + 2) * 3')
  // | "(" exp exp+ ")"                # apply    : f(a) == (f a)
  eq(3, '(add 1 2)', 'fn add a b: a + b')
  // | "[" exp* "]"                    # array    : [], [1 2]
  eq([], '[]')
  eq([1, 2], '[1 2]')
  eq([1, 2], '[1 1 2].set')
  eq(true, '[1].set[1]')
  eq(false, '[1].set[2]')
  // | "{" id* (id ":" exp)* "}"       # dictionary: {}, {one "two":2}
  eq({}, '{}')
  eq({one: 1, two: 2}, '{one "two":2}', 'let one 1')
  eq(['one', 'two'], '{one "two":2}.keys', 'let one 1')
  eq([1, 2], '{one "two":2}.values', 'let one 1')
  // | '"' [^"]* '"'                   # string   : "hi"
  eq('hi', '"hi"')
  // | '`' ("$" unit | [^"])* '`'      # template : `hi $user.name`
  eq('hi moa', '`hi $name`', 'let name "moa"')
  eq('a\nb', 'v', 'let v `a\nb`')
  // | '"' [^"]* '"'                   # regexp    : r"hi"
  eq(true, 'r"^h".test("hi")')
  eq(false, 'r"^h".test("bye")')
  // | id ("," id)* "=>" exp           # lambda   : a,b => a + b
  eq(1, 'f(a => 1)', 'fn f g: g()')
  eq(3, 'f(a,b => 3)', 'fn f g: g(1 2)')
  // | [0-9]+ ("." [0-9]+)?            # number   : 1, 0.5
  eq(1, '1')
  eq(0.5, '0.5')
  // | id                              # id       : name
  eq(1, 'a', 'let a 1')

  // qw(let var fn struct adt if return case match fail)
  eq(1, 'n', 'let n 1')
  eq(1, 'var n 0\nn+=1\nn')
  eq(1, 'f()', 'fn f: 1')
  eq({name: 'apple', price: 199}, 'item("apple" 199)', 'struct item:\n  name string\n  price int')
  eq(1, 'if true: return 1\n2')
  eq(2, 'if false: return 1\n2')
  eq(1, 'case true 1 2')
  eq(2, 'case false 1 2')
  eq(2, 'case false 1 true 2 3')
  eq(3, 'case false 1 false 2 3')
  eq(1, 'match a(1):\n  a v: v\n  b v: v.size\n  c: 0', 'adt ab:\n  a int\n  b string\n  c')
  eq(2, 'match b("hi"):\n  a v: v\n  b v: v.size\n  c: 0', 'adt ab:\n  a int\n  b string\n  c')
  eq(0, 'match c:\n  a v: v\n  b v: v.size\n  c: 0', 'adt ab:\n  a int\n  b string\n  c')
  error('hi', 'fail("hi")')
  error('Zero division error', '1/0')

  // priority of operators
  eq(3, '"hi".size + 1')
  eq(3, '1 + "hi".size')

  // string methods
  eq(1, '"a".size')
  // array methods
  eq(1, '[0].size')
  //eq([1], '[0].map(n => n + 1)')
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
