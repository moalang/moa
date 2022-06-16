'use strict'

Error.stackTraceLimit = 100

const fs = require('fs')
const str = o => typeof o === 'string' ? o : JSON.stringify(o)
const strs = o => Array.isArray(o) ? o.map(str).join(' ') : str(o)
const put = (...a) => process.stdout.write(strs(a))
const puts = (...a) => console.log(strs(a))
const dump = (...a) => a.map(o => console.dir(o, {depth: null}))
const trace = o => { dump(o); return o }
const reserves = 'let var fn struct when if unless for while continue break return fail p pp)'.split(' ')

function Token(code, pos, type) { this.code = code; this.pos = pos; this.type = type }
Token.prototype.toString = function() { return this.code }
function Group(tag, a) { this[tag] = true; this.a = a; this.type = undefined }
Group.prototype.toString = function() { return '(' + this.a.map(x => x.toString()).join(' ') + ')' }

const newToken = (code, pos, type) => new Token(code, pos, type)
const newArray = a => new Group('array', a)
const newStruct = a => new Group('struct', a.map(x => x.code ? ['=', x, x] : x))
const newStatement = a => new Group('statement', a)

const isOp2 = t => t && t.toString().match(/^[+\-*%/=><|&^]+$/)
const compile = source => {
  const tokens = tokenize(source)
  const nodes = infer(parse(tokens))
  const js = generate(nodes)
  return {tokens, nodes, js}
}

const tokenize = source => {
  const simplify = ts => {
    let pos = 0
    const safe = t => t === '{' ? '{{' : t === '}' ? '}}' : t
    ts = ts.map(code => { const t = newToken(safe(code), pos); pos += code.length; return t })
    ts = ts.filter(x => x.code.replace(/^ +/, ''))

    let nesting = 0
    let indent = 0
    const close = n => [...Array(n)].flatMap(_ => [';', '}', ';']).map(t => newToken(t, -1))
    const convert = t => {
      if (t == ':') {
        return newToken('{', t.pos)
      } else if (nesting === 0 && t.code.includes('\n')) {
        const before = indent
        indent = t.code.split('\n').slice(-1)[0].length
        if (indent % 2 !== 0) {
          throw Error(`Indentations must be multiple of two spaces. But this is ${JSON.stringify(indent)}`)
        }
        if (indent == before) {
          return newToken(';', t.pos)
        } else if (indent < before) {
          return close((before - indent) / 2)
        }
        return []
      } else if ('[('.includes(t.code)) {
        ++nesting
      } else if (')]'.includes(t.code)) {
        --nesting
      }
      return t
    }
    return ts.flatMap(convert).concat(close(indent / 2))
  }
  return simplify(source.split(/([ \n]+|[0-9]+(?:\.[0-9]+)?|[A-Za-z0-9_]+(?:,[A-Za-z0-9_]+)*|[+\-*%/=><|&^]+|"[^"]*"|`[^`]*`|[^\[\](){} \n;\.]+|.)/g))
}

const parse = tokens => {
  let pos = 0
  const many = (f, option, g) => {
    option = option || {}
    const a = []
    while (pos < tokens.length) {
      if (option.stop && option.stop(tokens[pos])) {
        ++pos
        break
      }
      a.push(f(tokens[pos]))
    }
    return g ? g(a) : a
  }
  const consume = t => reserves.includes(t.code) ? line() : exp()
  const line = () => many(exp, {stop: t => t == ';'}, a => a.length === 1 ? a[0] : a)
  const exp = () => {
    const lhs = atom()
    const t = tokens[pos]
    if (t && isOp2(t)) {
      ++pos
      return [t, lhs, exp()]
    } else {
      return lhs
    }
  }
  const atom = () => {
    const unit = bottom()
    const pred = tokens[pos]
    if (pred == '.') {
      ++pos
      return [tokens[pos-1], unit, atom()]
    } else if (pred == '(' && tokens[pos - 1].pos === pred.pos - tokens[pos - 1].code.length) {
      return [unit].concat(bottom())
    } else {
      return unit
    }
  }
  const bottom = () => {
    if (pos >= tokens.length) {
      return null
    }
    const token = tokens[pos++]
    const code = token.code
    if (code.match(/^[A-Za-z0-9_]+/) || code.startsWith('"') || code.startsWith('`')) {
      return token
    } else if ('}]);'.includes(code)) {
      return token
    } else if (token == '{{') {
      return many(exp, {stop: u => u == '}}'}, newStruct)
    } else if (token == '(') {
      return many(exp, {stop: u => u == ')'}, a => a.length === 1 ? a[0] : a)
    } else if (token == '[') {
      return many(exp, {stop: u => u == ']'}, newArray)
    } else if (token == '{') {
      return many(line, {stop: u => u == '}'}, newStatement)
    } else if (isOp2(code)) {
      return token
    } else {
      throw Error(`Unexpected token "${code}"`)
    }
  }
  return many(consume)
}

const infer = nodes => {
  let tvarSequence = 0
  function Type(params) {
    this.name = params.name || ''
    this.types = params.types || []
    this.args = params.args || []
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
  const tvar = () => new Type({name: (++tvarSequence).toString(), dynamic: true})
  const tlambda = (...types) => types.length === 1 ? types[0] : new Type({types})
  const ttype = (name, f) => {
    f ||= () => ({})
    const args = Array(f.length).fill().map(tvar)
    return new Type({name, args, props: f(...args)})
  }
  const tint = ttype('int')
  const tbool = ttype('bool')
  const tstring = ttype('string', () => ({
    size: tint
  }))
  const tarray = ttype('array', t => ({
    size: tint
  }))
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
      if (head == 'fn' || head == 'let') {
        const name = tail[0].code
        const args = tail.slice(1, -1).map(arg => (arg.type = tvar(), arg))
        const body = tail.slice(-1)[0]
        const newEnv = Object.assign({}, env)
        args.forEach(arg => newEnv[arg.code] = arg.type)
        const ret = analyze(body, newEnv, nonGeneric.concat(args.map(t => t.type.name)))
        const ft = tlambda(...args.map(t => t.type), ret)
        return env[name] = ft
      } else if (head == '=>') {
        const newEnv = Object.assign({}, env)
        const args = tail[0].code ? tail[0].code.split(',').map((s,i) => new Token(s, tail[0].pos + i, tvar())) : []
        args.map(arg => newEnv[arg.code] = arg.type)
        return tlambda(...args.map(t => t.type), analyze(tail[1], newEnv, nonGeneric.concat(args.map(t => t.type.name))))
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
        return t.args[0]
      } else if (tail.length) {
        const argv = tail.map(t => analyze(t, env, nonGeneric))
        const rt = (env.__cache[str(argv)] ||= tvar()) // fix tvar
        const ft = analyze(head, env, nonGeneric)
        unify(tlambda(...argv, rt), ft, node)
        return rt
      } else {
        return analyze(head, env, nonGeneric)
      }
    } else if (node.statement) {
      return node.a.map(x => analyze(x, env, nonGeneric)).slice(-1)[0]
    } else if (node.array) {
      const t = fresh(tarray)
      node.a.forEach(x => unify(analyze(x, env, nonGeneric), t.args[0]))
      return t
    } else if (node.struct) {
      const kvs = node.a.map(x => [x[1].code, analyze(x[2], env, nonGeneric)])
      const name = 'struct_' + kvs.flatMap(([k,v]) => [k, v.toString()]).join('_')
      return ttype(name, () => Object.fromEntries(kvs))
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
    '+': tlambda(tint, tint, tint),
    '-': tlambda(tint, tint, tint),
    '*': tlambda(tint, tint, tint),
    '/': tlambda(tint, tint, tint),
    '%': tlambda(tint, tint, tint),
    '<': tlambda(tint, tint, tbool),
    'if': tlambda(tbool, v1, v1, v1),
  }
  nodes.map(node => analyze(node, topEnv, []))
  return nodes
}

const generate = nodes => {
  const embedded = {
    'array_size': (o, name) => `${gen(o)}.length`,
    'string_size': (o, name) => `${gen(o)}.length`
  }
  const gen = o => o.array ? '[' + o.a.map(gen) + ']' :
    o.struct ? '({' + o.a.map(x => `${x[1].code}: ${gen(x[2])}`) + '})' :
    o.statement ? statement(o.a.map(gen)) :
    Array.isArray(o) ? apply(o) : o.code
  const isCond = args => ['if', 'unless'].includes(args[args.length - 2].code)
  const cond = (head, args) => args.length === 0 ? head.code :
    args[0] == 'if' ? `if (${gen(args[1])}) ${head}` :
    args[0] == 'unless' ? `if (!${gen(args[1])}) ${head}` :
    fail(`Unknown condition ${args}`)
  const addReturn = x => x.match(/^return|if|for|while/) ? x : 'return ' + x
  const statement = a => `(() => { ${[...a.slice(0, -1), addReturn(a[a.length - 1])].join(';')} })()`
  const block = a => a[0].statement ? '{' + a.slice(1).map(gen).join(';') + '}' : gen(a)
  const when = a => a.length === 1 ? a[0] : `${a[0]} ? ${1} : ` + when(a.slice(2))
  const apply = node => {
    const [head, ...args] = node
    if (Array.isArray(head) || head.struct || head.statement) {
      return args.length ? gen(head) + '(' + args.map(gen) + ')' : gen(head)
    } else if (head.array) {
      return args.length ? gen(head) + '[' + args.map(gen) + ']' : gen(head)
    } else if (isOp2(head)) {
      if (head == '=>') {
        return '((' + gen(args[0]) + ') => ' + gen(args[1]) + ')'
      } else {
        return '(' + gen(args[0]) + head + gen(args[1]) + ')'
      }
    } else if (head == '.') {
      const key = args[0].type.name + '_' + args[1]
      if (key in embedded) {
        return embedded[key](args[0], args[1], args.slice(2).map(gen))
      } else {
        if (Array.isArray(args[1])) {
          return `${gen(args[0])}.${args[1][0]}(${args[1].slice(1).map(gen)})`
        } else {
          return `${gen(args[0])}.${args[1]}`
        }
      }
    } else if (head == 'let') {
      return `const ${gen(args[0])} = ${gen(...args.slice(1))}`
    } else if (head == 'var') {
      return `var ${gen(args[0])} = ${args.length >= 2 ? gen(args.slice(1)) : 'undefined'}`
    } else if (head == 'fn') {
      return `const ${gen(args[0])} = (${args.slice(1, -1).map(gen)}) => ${gen(args[args.length - 1])}`
    } else if (head == 'struct') {
      const names = args[args.length - 1].slice(1).map(a => '_' + a[0])
      return `const ${gen(args[0])} = (${names}) => ({${names}})`
    } else if (head == 'if') {
      return `if (${gen(args[0])}) ${block(args[1])} ${args.length >= 3 ? gen(args.slice(2)) : ''}`
    } else if (head == 'elif') {
      return `else if (${gen(args[0])}) ${block(args[1])} ${args.length >= 3 ? gen(args.slice(2)) : ''}`
    } else if (head == 'else') {
      return `else ${block(args)}`
    } else if (head == 'for') {
      if (args.length == 3) {
        return `for (let ${args[0]} of ${gen(args[1])}) ${block(args[2])}`
      } else {
        throw Error(`Unknown for syntax ${args}`)
      }
    } else if (head == 'when') {
      return when(args)
    } else if (head == 'while') {
      return `while (${gen(args[0])}) ${block(args[1])}`
    } else if (head == 'continue' || head == 'break') {
      return cond(head, args)
    } else if (head == 'return') {
      if (args.length === 0) {
        return 'return'
      } else if (args.length === 1) {
        return `return ${gen(args[0])}`
      } else if (args.length === 2) {
        return cond('return', args)
      } else if (args.length === 3) {
        return cond(`return ${args[0]}`, args.slice(1))
      } else {
        throw Error(`Unknown return syntax ${args}`)
      }
    } else if (head == 'p' || head == 'pp') {
      if (isCond(args)) {
        return cond(`__${head}(${args.slice(0, -2).map(gen)})`, args.slice(-2))
      } else {
        return `__${head}(${args.map(gen)})`
      }
    } else {
      return `${head}(${args.map(gen)})`
    }
  }
  return nodes.map(gen).join(';\n')
}

const tester = () => {
  const showType = type => {
    const show = t => t.instance ? show(t.instance) : t.name || '(' + t.types.map(show).join(' ') + ')'
    const s = show(type)
    const o = {}
    const r = s.replace(/\d+/g, t => o[t] ||= Object.keys(o).length + 1)
    return r
  }

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
      const actual = showType(types.slice(-1)[0])
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
    const source = (defs || []).concat(`fn main:\n  ${exp.replace("\n", "\n  ")}`).join('\n')
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
      const unwrapToken = o => Array.isArray(o) ? o.map(unwrapToken) : o.a ? o.a.map(unwrapToken) : o.toString()
      puts('FAILURE')
      puts('source:', source)
      puts('js    :', js)
      puts('nodes :', nodes.map(unwrapToken))
      puts('tokens:', tokens.map(unwrapToken))
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
  inf('(1)', 'int')
  inf('(true)', 'bool')
  inf('(false)', 'bool')
  // exp
  inf('(+ 1 1)', 'int')
  inf('(< 1 1)', 'bool')
  // branch
  inf('(if true 1 2)', 'int')
  inf('(if true true true)', 'bool')
  // value
  inf('(fn value 1)', 'int')
  // lambda
  inf('(() => 1)', 'int')
  inf('(x => x + 1)', '(int int)')
  // simple function
  inf('(fn inc a (+ a 1))', '(int int)')
  inf('(fn add a b (+ a b))', '(int int int)')
  // generic function
  inf('(fn f a a)', '(1 1)')
  inf('(fn f a b a)', '(1 2 1)')
  inf('(fn f a b b)', '(1 2 2)')
  inf('(fn f a a) (f 1)', 'int')
  inf('(fn f a a) (f 1) (f true)', 'bool')
  // struct
  inf('{a=1}.a', 'int')
  inf('{a=1 b="hi"}.b', 'string')
  // string
  inf('"hi".size', 'int')
  // generic array
  inf('[].size', 'int')
  inf('[1](0)', 'int')
  inf('["hi"](0)', 'string')
  // combinations
  inf('(fn f x (+ x 1)) (fn g x (+ x 2)) (+ (f 1) (g 1))', 'int')
  inf('(fn _ f g x (g (f x)))', '((1 2) (2 3) 1 3)')
  inf('(fn _ x y z (x z (y z)))', '((1 2 3) (1 2) 1 3)')
  inf('(fn _ b x (if (x b) x (fn _ x b)))', '(1 (1 bool) (1 1))')
  inf('(fn _ x (if true x (if x true false)))', '(bool bool)')
  inf('(fn _ x y (if x x y))', '(bool bool bool)')
  inf('(fn _ n ((fn _ x (x (fn _ y y))) (fn _ f (f n))))', '(1 1)')
  inf('(fn _ x y (x y))', '((1 2) 1 2)')
  inf('(fn _ x y (x (y x)))', '((1 2) ((1 2) 1) 2)')
  inf('(fn _ h t f x (f h (t f x)))', '(1 ((1 2 3) 4 2) (1 2 3) 4 3)')
  inf('(fn _ x y (x (y x) (y x)))', '((1 1 2) ((1 1 2) 1) 2)')
  inf('(fn id x x) (fn f y (id (y id)))', '(((1 1) 2) 2)')
  inf('(fn id x x) (fn f (if (id true) (id 1) (id 2)))', 'int')
  inf('(fn f x (3)) (fn g (+ (f true) (f 4)))', 'int')
  inf('(fn f x x) (fn g y y) (fn h b (if b (f g) (g f)))', '(bool (1 1))')
  // type errors
  reject('(+ 1 true)')

  // -- Tests for executions
  // node:
  // | keywords exp+ (":" ("\n  " node)+)? cond? "\n"
  // | exp+ cond? "\n"
  // exp: unit (op2 exp)*
  check(3, '1 + 2')
  // unit: bottom ("." id ("(" exp+ ")")?)*
  check(1, '[1].size')
  check(2, '"hi".size')
  // bottom:
  // | "(" exp ")"                     # priority : 1 * (2 + 3)
  check(9, '(1 + 2) * 3')
  // | "[" exp* "]"                    # array    : [] [1 2]
  check([], '[]')
  check([1, 2], '[1 2]')
  check(2, '[1 2](1)')
  // | "{" tags "}"                    # struct   : {} {one two=2 (three)=3}
  check({}, '{}')
  check({key:1}, '{key=1}')
  check({key1:1, key2:2}, '{key1 key2=2}', 'let key1 1')
  // | '"' [^"]* '"'                   # string   : "hi"
  check('hi', '"hi"')
  check('${name}', '"${name}"')
  // | '`' ("${" unit "}" | [^"])* '`' # template : "hi {name}"
  check('hello moa', '`hello ${name}`', 'let name "moa"')
  // | id ("," id)* "=>" exp           # lambda   : a,b => a + b
  check('3', '(x => x + 1)(2)')
  check('3', '(a,b => a + b)(1 2)')
  // | [0-9]+ ("." [0-9]+)?            # number   : 1 0.5
  check(1, '1')
  check(1.5, '1.5')
  // | id ("(" exp+ ")")?              # id       : name f()
  check(1, 'v', 'let v 1')
  check(1, 'f()', 'fn f 1')
  check(2, 'inc(1)', 'fn inc x:\n  x + 1')
  check(3, 'add(1 2)', 'fn add a b:\n  a + b')
  // keyword: qw(let var fn struct if unless for while continue break return fail p pp)
  check(1, 'when true 1 2')
  check(2, 'when false 1 2')
  check(3, 'when false 1 false 2 3')

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
