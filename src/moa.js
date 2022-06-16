'use strict'

Error.stackTraceLimit = 100

function Token(code, type) {
  this.code = code
  this.type = type
}
function Type(params) {
  this.name = params.name || ''
  this.types = params.types || []
  this.targs = params.targs || []
  this.props = params.props || {}
  this.dynamic = params.dynamic || false
  this.instance = params.instance || undefined
}
Type.prototype.toString = function() {
  if (this.instance) {
    return this.instance.toString()
  } else {
    const types = this.types.map(x => x.toString()).join(' ')
    return this.name + (types ? `(${types})` : '')
  }
}
const showType = type => {
  const pretty = a => a.length === 0 ? '' : `(${a.map(show).join(' ')})`
  const show = t => t.instance ? show(t.instance) : t.name + pretty(t.targs) || pretty(t.types)
  return show(type)
}
const simplifyTypeString = s => {
  const o = {}
  return s.replace(/\d+/g, t => o[t] ||= Object.keys(o).length + 1)
}
const showTypeIfPossible = o => typeof o === 'object' && o.type ? ':' + showType(o.type) : ''
Token.prototype.toString = Token.prototype.valueOf = function() { return this.code }
'startsWith endsWith match replace'.split(' ').map(s => Token.prototype[s] = function(...a) { return this.code[s](...a) } )

const fs = require('fs')
const str = o => Array.isArray(o) ? o.map(str).join(' ') :
  typeof o === 'string' ? o :
  typeof o === 'object' && o.constructor === Token ? o.toString() :
  typeof o === 'object' && o.constructor === Type ? showType(o) :
  JSON.stringify(o)
const put = (...a) => process.stdout.write(str(a))
const puts = (...a) => console.log(...a.map(str))
const dump = o => Array.isArray(o) ? (o.type ? '[' + o.map(dump).join(' ') + ']' + showTypeIfPossible(o) : o.map(dump)) :
  typeof o === 'object' && o.constructor === Token ? o.code + showTypeIfPossible(o) :
  str(o)
const trace = o => { console.dir(dump(o), {depth: null}); return o }

const isOp1 = t => t == '!'
const isOp2 = t => t && t.toString().match(/^[+\-*%/=><|&^]+$/)
const isAssign = t => t.endsWith('=')
const compile = source => {
  const tokens = tokenize(source)
  const nodes = infer(parse(tokens))
  const js = generate(nodes)
  return {tokens, nodes, js}
}

const tokenize = source => {
  const simplify = tokens => {
    let nesting = 0
    let indent = 0
    const close = n => [...Array(n)].flatMap(_ => ['__eol', '__eob', '__eol'])
    const convert = (token, i) => {
      if (!token || token.startsWith('#') || !token.replace(/^ +$/, '')) {
        return []
      }

      if (nesting === 0 && token.match(/^[ \n]+/)) {
        const before = indent
        indent = token.split('\n').slice(-1)[0].length
        if (indent % 2 !== 0) {
          throw Error(`Indent error: '${JSON.stringify(indent)}'`)
        }
        if (indent === before) {
          return '__eol'
        } else if (indent < before) {
          return close((before - indent) / 2)
        }
        return []
      } else if ('{[('.includes(token)) {
        ++nesting
      } else if (')]}'.includes(token)) {
        --nesting
      }

      const prev = tokens[i - 1] || ''
      const pred = tokens[i + 1] || ''
      if (token === '(' && prev && prev.match(/^[A-Za-z0-9_)\]]/)) {
        return '__call'
      }
      if (token === '[' && prev && prev.match(/^[A-Za-z0-9_)\]]/)) {
        return '__at'
      }
      if (token === ':' && pred && !pred.includes('\n')) {
        return '__line'
      }
      return token
    }
    return tokens.flatMap(convert).concat(close(indent / 2)).map(t => new Token(t))
  }
  return simplify(source.split(/((?: |\n|#[^\n]*)+|[0-9]+(?:\.[0-9]+)?|[A-Za-z0-9_]+(?:,[A-Za-z0-9_]+)*|[+\-*%/=><|&^]+|"[^"]*"|`[^`]*`|.)/g).filter(x => x))
}

const parse = tokens => {
  let pos = 0
  const many = (f, stop, g) => {
    const a = []
    while (pos < tokens.length) {
      const token = tokens[pos]
      if (stop == token) {
        ++pos
        break
      } else if (token == '__line') {
        ++pos
        a.push(many(exp))
      } else if (token.startsWith('__')) {
        break
      } else {
        a.push(f(token))
      }
    }
    return g ? g(a) : a
  }
  const line = () => many(exp, '__eol')
  const exp = () => glue(bottom())
  const glue = lhs => {
    const pred = tokens[pos++]
    if (isOp1(lhs)) {
      --pos
      return [lhs, exp()]
    } else if (isOp2(pred)) {
      return [pred, lhs, exp()]
    } else if (pred == '.') {
      return [tokens[pos-1], lhs, exp()]
    } else if (pred == '__call') {
      return glue(many(exp, ')', a => [new Token('__call'), lhs, ...a]))
    } else if (pred == '__at') {
      return glue([pred, lhs, many(exp, ']')])
    } else {
      --pos
      return lhs
    }
  }
  const bottom = () => {
    if (pos >= tokens.length) {
      return null
    }
    const token = tokens[pos++]
    if (token.match(/^[A-Za-z0-9_"`}\])]/) || isOp1(token) || isOp2(token)) {
      return token
    } else if (token == '{') {
      return many(exp, '}', a => [new Token('__struct'), ...a])
    } else if (token == '(') {
      return many(exp, ')')
    } else if (token == '[') {
      return many(exp, ']', a => [new Token('__array'), ...a])
    } else if (token == ':') {
      return many(line, '__eob', a => [new Token('__block'), ...a])
    } else {
      throw Error(`Unexpected token "${token}"`)
    }
  }
  const unnest = a => !Array.isArray(a) ? a : a.length === 1 ? unnest(a[0]) : a.map(unnest)
  return many(line).map(unnest)
}

const infer = nodes => {
  let tvarSequence = 0
  const tvar = () => new Type({name: (++tvarSequence).toString(), dynamic: true})
  const tlambda = (...types) => types.length === 1 ? types[0] : new Type({types})
  const ttype = (name, f) => {
    f ||= () => ({})
    const targs = Array(f.length).fill().map(tvar)
    return new Type({name, targs, props: f(...targs)})
  }
  const tint = ttype('int')
  const tbool = ttype('bool')
  const tstring = ttype('string', () => ({ size: tint }))
  const tarray = ttype('array', t => ({ size: tint }))
  const fresh = (type, nonGeneric) => {
    const d = {}
    const rec = t => {
      const p = prune(t)
      if (p.dynamic) {
        if (nonGeneric.includes(p.name)) {
          return p
        } else {
        return d[p.name] ||= tvar()
        }
      } else {
        const q = new Type(p)
        q.types = q.types.map(rec)
        q.targs = q.targs.map(rec)
        return q
      }
    }
    return rec(type)
  }
  const unify = (a, b, node) => {
    a = prune(a)
    b = prune(b)
    if (a.dynamic) {
      if (a.name !== b.name) {
        a.instance = b
      }
    } else if (b.dynamic) {
      unify(b, a, node)
    } else {
      if (a.name !== b.name || a.types.length !== b.types.length) {
        throw Error(`type miss match "${a}" and "${b}" around ${node}`)
      }
      a.types.forEach((t,i) => unify(t, b.types[i], node))
    }
  }
  const v1 = tvar()
  const prune = t => (t.dynamic && t.instance) ? t.instance = prune(t.instance) : t
  const analyze = (node, env, nonGeneric) => node.type = _analyze(node, env, nonGeneric)
  const _analyze = (node, env, nonGeneric) => {
    if (Array.isArray(node)) {
      let [head,...tail] = node
      if (head == 'fn' || head == 'let' || head == 'var') {
        const name = tail[0].code
        const targs = tail.slice(1, -1).map(arg => (arg.type = tvar(), arg))
        const body = tail.slice(-1)[0]
        const newEnv = Object.assign({}, env)
        targs.forEach(arg => newEnv[arg.code] = arg.type)
        const ret = analyze(body, newEnv, nonGeneric.concat(targs.map(t => t.type.name)))
        const ft = tlambda(...targs.map(t => t.type), ret)
        return env[name] = ft
      } else if (head == 'struct') {
        const name = tail[0].code
        const kvs = tail[1].slice(1).map(([name, type]) => [name.code, ttype(type.code)])
        const ret = ttype(name, () => Object.fromEntries(kvs))
        const ft = tlambda(...kvs.map(kv => kv[1]), ret)
        return env[name] = ft
      } else if (head == '__call') {
        return _analyze(tail[0], env, nonGeneric)
      } else if (head == '__block') {
        predict(env, tail)
        return tail.map(line => analyze(line, env, nonGeneric)).slice(-1)[0]
      } else if (head == '__array') {
        const ta = fresh(tarray, nonGeneric)
        tail.map(x => unify(ta.targs[0], analyze(x, env, nonGeneric), x))
        return ta
      } else if (head == '__at') {
        const ary = analyze(tail[0], env, nonGeneric)
        const index = analyze(tail[1], env, nonGeneric)
        unify(tint, index)
        return ary.targs[0]
      } else if (head == '__struct') {
        const kvs = tail.map(x => Array.isArray(x) ?
          [x[1].code, analyze(x[2], env, nonGeneric)] :
          [x.code, analyze(x, env, nonGeneric)])
        const name = 'struct_' + kvs.flatMap(([k,v]) => [k, v.toString()]).join('_')
        return ttype(name, () => Object.fromEntries(kvs))
      } else if (head == '=>') {
        const newEnv = Object.assign({}, env)
        const targs = tail[0].code ? tail[0].code.split(',').map((s,i) => new Token(s, tvar())) : []
        targs.map(arg => newEnv[arg.code] = arg.type)
        return tlambda(...targs.map(t => t.type), analyze(tail[1], newEnv, nonGeneric.concat(targs.map(t => t.type.name))))
      } else if (head == '.') {
        return analyze(tail[0], env, nonGeneric).props[tail[1].code]
      } else if (head == 'when') {
        for (let i=0; i<tail.length - 1; i+=2) {
          unify(tbool, analyze(tail[i], env, nonGeneric))
        }
        const ret = tvar()
        for (let i=1; i<tail.length; i+=2) {
          unify(ret, analyze(tail[i], env, nonGeneric))
        }
        return ret
      } else if (head.array && tail.length) {
        const t = analyze(head, env, nonGeneric)
        if (tail.length !== 1) {
          throw new Error(`Array calling should take 1 argument but gave ${tail.length}`)
        }
        unify(tint, analyze(tail[0], env, nonGeneric))
        return t.targs[0]
      } else if (tail.length) {
        const argv = tail.map(t => analyze(t, env, nonGeneric))
        const rt = (env.__cache[str(argv)] ||= tvar()) // fix tvar
        const ft = analyze(head, env, nonGeneric)
        unify(tlambda(...argv, rt), ft, node)
        return rt
      } else {
        return analyze(head, env, nonGeneric)
      }
    } else if (node == '__array') {
      return fresh(tarray, nonGeneric)
    } else if (node == '__struct') {
      return ttype('', () => ({}))
    } else {
      const v = node.code
      if (v.match(/^[0-9]/)) {
        return tint
      } else if (v.match(/^["`]/)) {
        return tstring
      } else if (v in env) {
        return fresh(env[v], nonGeneric)
      } else {
        throw Error(`Not found ${v} in ${Object.keys(env)}`)
      }
    }
  }
  const topEnv = {
    __cache: {},
    'true': tbool,
    'false': tbool,
    '<': tlambda(tint, tint, tbool),
    '!': tlambda(tbool, tbool),
    '&&': tlambda(tbool, tbool, tbool),
    'if': tlambda(tbool, v1, v1, v1),
  }
  '+ - * / % += -= *= /= %='.split(' ').map(op => topEnv[op] = tlambda(tint, tint, tint))
  const defs = 'fn struct'.split(' ')
  const predict = (env, nodes) => nodes.map(x => Array.isArray(x) && defs.includes(x[0].code) ? env[x[1]] = tvar() : null)
  predict(topEnv, nodes)
  nodes.map(node => analyze(node, topEnv, []))
  return nodes
}

const generate = nodes => {
  const embedded = {
    'array_size': (o, name) => `${gen(o)}.length`,
    'string_size': (o, name) => `${gen(o)}.length`
  }
  const gen = o => Array.isArray(o) ? apply(o) :
    o == '__array' ? '[]' :
    o == '__struct' ? '({})' :
    o.startsWith('`') ? template(o) : o
  const template = s => s.replace(/\$[^ `]+/g, x => '${' + x.slice(1) + '}')
  const when = a => a.length === 1 ? a[0] : `${a[0]} ? ${a[1]} : ` + when(a.slice(2))
  const statement = a => a.map((v, i) => a.length - 1 === i ? 'return ' + v : v)
  const apply = node => {
    const [head, ...tail] = node
    const args = tail.map(gen)
    // internal marks
    if (head == '__call') {
      return args[0] + `(${args.slice(1)})`
    } else if (head == '__block') {
      return args.length === 1 ? args[0] : '{\n  ' + statement(args).join('\n  ') + '\n}'
    } else if (head == '__array') {
      return `[${args.join(', ')}]`
    } else if (head == '__struct') {
      const kvs = tail.map(o => Array.isArray(o) ? `${o[1]}:${o[2]}` : o).join(',')
      return `({${kvs}})`
    } else if (head == '__at') {
      return args[0] + `[${args[1]}]`

    // operators
    } else if (head == '.') {
      const key = showType(tail[0].type) + '_' + args[1]
      if (key in embedded) {
        return embedded[key](args[0], args[1], args.slice(2).map(gen))
      } else {
        if (Array.isArray(args[1])) {
          return `${gen(args[0])}.${args[1][0]}(${args[1].slice(1).map(gen)})`
        } else {
          return `${gen(args[0])}.${args[1]}`
        }
      }
    } else if (isOp1(head)) {
      return '(' + head + args.join('') + ')'
    } else if (isOp2(head)) {
      const [lhs, rhs] = args
      if (head == '=>') {
        return `((${lhs}) => ${rhs})`
      } else if (isAssign(head)) {
        return `${lhs} ${head} ${rhs}`
      } else {
        return `(${lhs} ${head} ${rhs})`
      }

    // keywords
    } else if (head == 'let') {
      return `const ${args[0]} = ${args[1]}`
    } else if (head == 'var') {
      return `let ${args[0]} = ${args[1]}`
    } else if (head == 'fn') {
      return `const ${args[0]} = (${args.slice(1, -1)}) => ${args[args.length - 1]}`
    } else if (head == 'struct') {
      const names = tail[1].slice(1).map(x => x[0])
      return `const ${tail[0]} = (${names}) => ({${names}})`
    } else if (head == 'adt') {
      const names = tail[1].slice(1).map(x => x[0])
      return names.map(name => `const ${name} = __val => ({__tag:'${name}',__val})`).join('\n')
    } else if (head == 'match') {
      const error = '(()=>{throw Error(`Unmatch error: "${__.__tag}"`)})()'
      const match = tail[1].slice(1).map(a => `__.__tag === '${a[0]}' ? (${gen(a[1])} => ${gen(a[2])})(__.__val) : `).join('\n  ') + error
      return `(__ => ${match})(${args[0]})`
    } else if (head == 'when') {
      return when(args)
    } else if (head == 'if') {
      return `${args[0]} ? ${args[1]} : null`
    } else if (head == 'p') {
      return `__p(${args})`

    // general
    } else {
      return `${head}(${args})`
    }
  }
  return nodes.map(gen).join(';\n')
}


const tester = () => {
  const reject = source => {
    try {
      infer(parse(tokenize(source)))
    } catch (e) {
      if (e.message.match(/^type miss match/)) {
        process.stdout.write('.')
        return
      }
    }
    puts('Failed')
    puts('source:', source)
  }

  const inf = (source, expect) => {
    try {
      let types = infer(parse(tokenize(source))).map(node => node.type)
      const actual = simplifyTypeString(showType(types.slice(-1)[0]))
      if (str(actual) === str(expect)) {
        process.stdout.write('.')
      } else {
        puts('Failed')
        puts('expect:', expect)
        puts('actual:', actual)
        puts('source:', source)
        process.exit(1)
      }
    } catch (e) {
      puts('Failed')
      puts('  source:', source)
      console.error(e)
      process.exit(2)
    }
  }

  const check = (expect, exp, ...defs) => {
    const source = (defs || []).concat(`fn main:\n  ${exp.replace(/\n/g, "\n  ")}`).join('\n')
    const {js, nodes, tokens} = compile(source)
    let actual
    try {
      actual = eval(js + '\nmain()')
    } catch(e) {
      actual = e.stack
    }
    if (str(actual) === str(expect)) {
      put('.')
    } else {
      puts('FAILURE')
      puts('source:', source)
      puts('js    :', js)
      puts('nodes :', nodes)
      puts('tokens:', tokens)
      puts('expect:', expect)
      puts('actual:', actual)
      process.exit(3)
    }
  }

  return {inf, reject, check}
}

const testAll = () => {
  const {inf, reject, check} = tester()

  // -- Tests for type inference
  // primitives
  inf('1', 'int')
  inf('true', 'bool')
  inf('false', 'bool')
  // exp
  inf('+ 1 1', 'int')
  inf('< 1 1', 'bool')
  // branch
  inf('if true 1 2', 'int')
  inf('if true true true', 'bool')
  // value
  inf('fn value 1', 'int')
  // lambda
  inf('() => 1', 'int')
  inf('x => x + 1', '(int int)')
  // simple function
  inf('fn inc a (+ a 1)', '(int int)')
  inf('fn add a b (+ a b)', '(int int int)')
  // generic function
  inf('fn f a a', '(1 1)')
  inf('fn f a b a', '(1 2 1)')
  inf('fn f a b b', '(1 2 2)')
  inf('fn f a a\nf 1', 'int')
  inf('fn f a a\nf 1\nf true', 'bool')
  // struct
  inf('{a=1}.a', 'int')
  inf('{a=1 b="hi"}.b', 'string')
  // string
  inf('"hi".size', 'int')
  // generic array
  inf('[].size', 'int')
  inf('[1][0]', 'int')
  inf('["hi"][0]', 'string')
  // combinations
  inf('fn f x (+ x 1)\nfn g x (+ x 2)\n(+ (f 1) (g 1))', 'int')
  inf('fn _ f g x (g (f x))', '((1 2) (2 3) 1 3)')
  inf('fn _ x y z (x z (y z))', '((1 2 3) (1 2) 1 3)')
  inf('fn _ b x (if (x b) x (x => b))', '(1 (1 bool) (1 1))')
  inf('fn _ x (if true x (if x true false))', '(bool bool)')
  inf('fn _ x y (if x x y)', '(bool bool bool)')
  inf('fn _ n ((x => (x (y => y))) (f => (f n)))', '(1 1)')
  inf('fn _ x y (x y)', '((1 2) 1 2)')
  inf('fn _ x y (x (y x))', '((1 2) ((1 2) 1) 2)')
  inf('fn _ h t f x (f h (t f x))', '(1 ((1 2 3) 4 2) (1 2 3) 4 3)')
  inf('fn _ x y (x (y x) (y x))', '((1 1 2) ((1 1 2) 1) 2)')
  inf('fn id x x\nfn f y (id (y id))', '(((1 1) 2) 2)')
  inf('fn id x x\nfn f (if (id true) (id 1) (id 2))', 'int')
  inf('fn f x (3)\nfn g (+ (f true) (f 4))', 'int')
  inf('fn f x x\nfn g y y\nfn h b (if b (f g) (g f))', '(bool (1 1))')
  // type errors
  reject('(+ 1 true)')

  // -- Tests for executions
  // top: node*
  check(3, 'a + b()', 'let a 1\nfn b 2')
  // node: exp+ ("\n" | (":\n" ("  " node)+)?)
  // exp: unit (op2 exp)*
  check(7, '1 + 2 * 3')
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
  // | "{" id* (id "=" exp)* "}"       # struct   : {}, {one two=2}
  check({}, '{}')
  check({one: 1, two: 2}, '{one two=2}', 'let one 1')
  // | '"' [^"]* '"'                   # string   : "hi"
  check('hi', '"hi"')
  check('hi', '"hi"')
  // | '`' ("$" unit | [^"])* '`'      # template : `hi $user.name`
  check('hi moa', '`hi $name`', 'let name "moa"')
  check('a\nb', 'v', 'let v `a\nb`')
  // | id ("," id)* "=>" exp           # lambda   : a,b => a + b
  check(1, 'f(a => 1)', 'fn f g: g()')
  check(3, 'f(a,b => 3)', 'fn f g: g(1 2)')
  // | [0-9]+ ("." [0-9]+)?            # number   : 1, 0.5
  check(1, '1')
  check(0.5, '0.5')
  // | id                              # id       : name
  check(1, 'a', 'let a 1')

  // qw(let var fn struct adt if when match)
  check(1, 'n', 'let n 1')
  check(1, 'var n 0\nn+=1\nn')
  check(1, 'f()', 'fn f: 1')
  check({name: 'apple', price: 199}, 'item("apple" 199)', 'struct item:\n  name string\n  price int')
  //check(1, 'if true: p 1')
  //check(null, 'if false: p 1')
  check(1, 'when true 1 2')
  check(2, 'when false 1 2')
  check(2, 'when false 1 true 2 3')
  check(3, 'when false 1 false 2 3')
  //check(3, 'match a(1):\n  a v: v + 2\n  b v: v', 'adt ab:\n  a int\n  b string')
  //check(2, 'match b("hi"):\n  a v: v\n  b v: v.size', 'adt ab:\n  a int\n  b string')

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
  rl.on('line', (line) => {
    const cmd = line.trim()
    if (['exit', 'quit', 'q'].includes(cmd)) {
      rl.close()
      return
    }
    let {js} = compile(cmd)
    js = js.replace(/^let |const /g, 'global.')
    try {
      puts(eval(js))
    } catch (e) {
      puts(e.stack)
    }
    puts('js:', js)
    rl.prompt()
  }).on('close', () => {
    puts('Bye👋')
  })
}

const main = () => {
  const paths = process.argv.slice(2)
  if (paths.length === 0) {
    interactive()
  } else if (paths[0] === '--test') {
    testAll()
  } else {
    const moa = paths.map(path => fs.readFileSync(path, 'utf-8')).join('\n\n')
    puts(compile(moa).js)
  }
}

main()
