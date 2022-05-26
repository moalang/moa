'use strict'

const fs = require('fs')
const str = o => typeof o === 'string' ? o : JSON.stringify(o)
const strs = o => Array.isArray(o) ? o.map(str).join(' ') : str(o)
const put = (...a) => process.stdout.write(strs(a))
const puts = (...a) => console.log(strs(a))
const dump = (...a) => a.map(o => console.dir(o, {depth: null}))
const trace = o => { dump(o); return o }
const reserves = 'let var fn struct if unless for while continue break return fail p pp)'.split(' ')

const embeddedJs = (() => {
const __error = (message, obj) => { throw Object.assign(Error(message), obj) }
const __now = () => {
  const d = new Date()
  const pnow = performance.now()
  const _year = d.getFullYear()
  const _month = d.getMonth() + 1
  const _day = d.getDate()
  const _hour = d.getHours()
  const _minute = d.getMinutes()
  const _second = d.getSeconds()
  const _string = `${_year}/${('0' + _month).slice(-2)}/${('0' + _day).slice(-2)} ${('0' + _hour).slice(-2)}:${('0' + _minute).slice(-2)}:${('0' + _second).slice(-2)}`
  const _elapsed = () => Math.floor((performance.now() - pnow) * 10) / 10 + 'ms'
  return { _year, _month, _day, _hour, _minute, _second, _string, _elapsed }
}
const __p = (...args) => console.log(...args.map(x => ['string', 'number'].includes(typeof x) ? x : JSON.stringify(x)))
const __pp = (...args) => console.log(...args.map(x => JSON.stringify(x, null, 2)))
}).toString().slice(8, -1).trim()

function Token(code, pos) { this.code = code; this.pos = pos }
Token.prototype.update = function(code) { this.code = code; return this}
Token.prototype.toString = function() { return this.code }
function Group(tag, a) { this[tag] = true; this.a = a }
Token.prototype.toString = function() { return this.code }
const newToken = (code, pos) => new Token(code, pos)
const newArray = a => new Group('array', a)
const newStruct = a => new Group('struct', a.map(x => x.code ? ['=', x, x] : x))
const newStatement = a => new Group('statement', a)
const isOp2 = t => t && t.toString().match(/^[+\-*%/=><|&^]+$/)
const compile = source => {
  const tokens = tokenize(source)
  const nodes = (parse(tokens))
  const js = generate(nodes)
  return {tokens, nodes, js}
}
const tokenize = source => {
  const simplify = ts => {
    let nesting = 0
    let indent = 0
    let pos = -ts[0].length
    const wrap = t => newToken(t, pos)
    const close = n => [...Array(n)].flatMap(_ => [';', '}', ';']).map(t => newToken(t, -1))
    const convert = t => {
      pos += t.length
      if (t === ':') {
        return wrap('{')
      } else if (nesting === 0 && t.includes('\n')) {
        const before = indent
        indent = t.split('\n').slice(-1)[0].length
        if (indent % 2 !== 0) {
          throw Error(`Indentations must be multiple of two spaces. But this is ${JSON.stringify(indent)}`)
        }
        if (indent == before) {
          return wrap(';')
        } else if (indent < before) {
          return close((before - indent) / 2)
        }
        return []
      } else if ('[('.includes(t) || t.endsWith('(')) {
        ++nesting
      } else if (')]'.includes(t)) {
        --nesting
      }
      return wrap(t)
    }
    ts = ts.map(t => t === '{' ? '{{' : t === '}' ? '}}' : t)
    return ts.flatMap(convert).concat(close(indent / 2))
  }
  return simplify(source.split(/([ \n]+|[0-9]+(?:\.[0-9]+)?|[A-Za-z0-9_]+(?:,[A-Za-z0-9_]+)|[A-Za-z0-9_]+\(?|[+\-*%/=><|&^]+|"[^"]*"|`[^`]*`|[^\[\](){} \n;\.]+|.)/g).filter(x => x.replace(/^ +$/, '')))
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
  const line = () => many(exp, {stop: t => t == ';'})
  const exp = () => ((lhs, t) => isOp2(t) ? ++pos && [t, lhs, exp()] : lhs)(atom(), tokens[pos])
  const atom = () => {
    const unit = bottom()
    if (tokens[pos] == '.') {
      ++pos
      return [tokens[pos-1], unit, atom()]
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
    if (code.match(/^[A-Za-z0-9_]+\($/)) {
      return  many(exp, {stop: u => u == ')'}, a => a.length ? [token.update(code.slice(0, -1)),  ...a] : token.update(code + ')'))
    } else if (code.match(/^[A-Za-z0-9_]+/) || code.startsWith('"') || code.startsWith('`')) {
      return token
    } else if ('}]);'.includes(code)) {
      return token
    } else if (token == '{{') {
      return many(exp, {stop: u => u == '}}'}, newStruct)
    } else if (token == '(') {
      return many(exp, {stop: u => u == ')'})
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
  function Type(name, types, f, dynamic) {
    this.name = name
    this.types = types || []
    this.args = f ? Array(f.length).fill().map(tvar) : []
    this.props = f ? f(...this.args) : {}
    this.dynamic = dynamic || false
  }
  Type.prototype.toString = function() {
    const types = this.types.map(x => x.toString()).join(' ')
    return this.name + (types ? `(${types}.join(' '))` : '')
  }
  const tvar = () => new Type((++tvarSequence).toString(), [], null, true)
  const tlambda = (...types) => types.length === 1 ? types[0] : new Type('', types)
  const ttype = (name, f) => new Type(name, [], f)
  const tint = ttype('int')
  const tbool = ttype('bool')
  const tstring = ttype('string', () => ({
    size: tint
  }))
  const tarray = ttype('array') // TODO: generics
  const fresh = (type, nonGeneric) => {
    const d = {}
    const rec = t => {
      const p = prune(t)
      return p.dynamic ?
        (nonGeneric.includes(p.name) ? p : d[p.name] ||= tvar()) :
        ({name: p.name, types: p.types.map(rec)})
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
  const analyse = (node, env, nonGeneric) => node.type = _analyse(node, env, nonGeneric)
  const _analyse = (node, env, nonGeneric) => {
    if (Array.isArray(node)) {
      let [head,...tail] = node
      if (head == 'fn' || head == 'let') {
        const name = tail[0].code
        const args = tail.slice(1, -1).map(arg => (arg.type = tvar(), arg))
        const body = tail.slice(-1)[0]
        const newEnv = Object.assign({}, env)
        args.forEach(arg => newEnv[arg.code] = arg.type)
        const ret = analyse(body, newEnv, nonGeneric.concat(args.map(t => t.type.name)))
        const ft = tlambda(...args.map(t => t.type), ret)
        return env[name] = ft
      } else if (head == '=>') {
        const newEnv = Object.assign({}, env)
        const args = [tail[0]]
        args[0].type = tvar()
        args.forEach(arg => newEnv[arg.code] = arg.type)
        return analyse(tail[1], newEnv, nonGeneric.concat(args.map(t => t.type.name)))
      } else if (head == '.') {
        return analyse(tail[0]).props[tail[1].code]
      } else if (tail.length) {
        const argv = tail.map(t => analyse(t, env, nonGeneric))
        const rt = (env.__cache[str(argv)] ||= tvar()) // fix tvar
        const ft = analyse(head, env, nonGeneric)
        unify(tlambda(...argv, rt), ft, node)
        return rt
      } else {
        return analyse(head, env, nonGeneric)
      }
    } else if (node.statement || node.array) {
      return node.a.map(x => analyse(x, env, nonGeneric)).slice(-1)[0] || tarray
    } else if (node.struct) {
      const kvs = node.a.map(x => [x[1].code, analyse(x[2])])
      const name = 'struct_' + kvs.flatMap(([k,v]) => [k, v.toString()]).join('_')
      return ttype(name, () => Object.fromEntries(kvs))
    } else {
      const v = node.code
      if (v.match(/^[0-9]/)) {
        return tint
      } else if (v.match(/^["`]/)) {
        return tstring
      } else if (env[v]) {
        return fresh(env[v], nonGeneric)
      } else {
        throw Error(`Not found ${v} in env`, {v,node,env})
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
  nodes.map(node => analyse(node, topEnv, []))
  return nodes
}

const generate = nodes => {
  const gen = o => o.array ? '[' + o.a.map(gen) + ']' :
    o.struct ? '({' + o.a.map(x => `${x[1].code}: ${gen(x[2])}`) + '})' :
    o.statement ? statement(o.a.map(gen)) :
    Array.isArray(o) ? (o.length === 1 ? gen(o[0]) : apply(o)) : o.code
  const isCond = args => ['if', 'unless'].includes(args[args.length - 2].code)
  const cond = (head, args) => args.length === 0 ? head.code :
    args[0] == 'if' ? `if (${gen(args[1])}) ${head}` :
    args[0] == 'unless' ? `if (!${gen(args[1])}) ${head}` :
    fail(`Unknown condition ${args}`)
  const addReturn = x => x.match(/^return|if|for|while/) ? x : 'return ' + x
  const statement = a => `(() => { ${[...a.slice(0, -1), addReturn(a[a.length - 1])].join(';')} })()`
  const block = a => a[0].statement ? '{' + a.slice(1).map(gen).join(';') + '}' : gen(a)
  const apply = ([head, ...args]) => {
    if (Array.isArray(head)) {
      return gen(head) + (args.length ? '(' + args.flatMap(x => x).map(gen) + ')' : '')
    } else if (isOp2(head)) {
      if (head == '=>') {
        return '((' + gen(args[0]) + ') => ' + gen(args[1]) + ')'
      } else {
        return '(' + gen(args[0]) + head + gen(args[1]) + ')'
      }
    } else if (head == '.') {
      if (Array.isArray(args[1])) {
        return `${gen(args[0])}._${args[1][0]}(${args[1].slice(1).map(gen)})`
      } else {
        return `${gen(args[0])}._${args[1]}`
      }
    } else if (head == ':') {
      const key = s => s.startsWith('$') ? `[${s.slice(1)}]` : s
      return '({' + Array.from({length: args.length/2}, (_, i) => key(args[i*2]) + ':' + args[i*2+1]).join(',') + '})'
    } else if (head == 'let') {
      return `const ${gen(args[0])} = ${gen(args.slice(1))}`
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

const testAll = () => {
  testInference()
  testJavaScript()
  puts('ok')
}

const testInference = () => {
  const showType = type => {
    const show = t => t.instance ? show(t.instance) : t.name || '(' + t.types.map(show).join(' ') + ')'
    const s = show(type)
    const o = {}
    const r = s.replace(/\d+/g, t => o[t] ||= Object.keys(o).length + 1)
    return r
  }

  const reject = src => {
    try {
      infer(parse(tokenize(src)))
    } catch (e) {
      if (e.message.match(/^type miss match/)) {
        process.stdout.write('.')
        return
      }
    }
    puts('Failed')
    puts('src:', src)
  }
  const inf = (src, expect) => {
    try {
      const types = infer(parse(tokenize(src))).map(node => node.type)
      const actual = showType(types.slice(-1)[0])
      if (str(actual) === str(expect)) {
        process.stdout.write('.')
      } else {
        puts('Failed')
        puts('expect:', expect)
        puts('actual:', actual)
        puts('   src:', src)
        process.exit(1)
      }
    } catch (e) {
      puts('Failed')
      puts('  src:', src)
      console.error(e)
      process.exit(2)
    }
  }

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

  // embedded types
  inf('"hi".size', 'int')

  // generic array
  //inf('[]', '1')

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
}


const testJavaScript = () => {
  const check = (expect, exp, ...defs) => {
    const source = (defs || []).concat(`fn main:\n  ${exp.replace("\n", "\n  ")}`).join('\n')
    const {js, nodes, tokens} = compile(source)
    let actual
    try {
      const __stdout = []
      const __p = (...a) => { __stdout.push(a.map(str).join(' ')) }
      const __pp = (...a) => { __stdout.push(a.map(x => JSON.stringify(x, null, 2)).join(' ')) }
      actual = eval(js + '\nmain()')
      if (actual === undefined) {
        actual = ''
      }
      if (__stdout.length) {
        actual = actual + __stdout.join('\n')
      }
    } catch(e) {
      actual = e.stack
    }
    if (str(actual) === str(expect)) {
      put('.')
    } else {
      const unwrapToken = o => Array.isArray(o) ? o.map(unwrapToken) : o.code
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

  // node:
  // | keywords exp+ (":" ("\n  " node)+)? cond? "\n"
  // | exp+ cond? "\n"
  // exp: unit (op2 exp)*
  // unit: bottom ("." id ("(" exp+ ")")?)*
  //check(2, '"hi".size')
  // bottom:
  // | "(" exp ")"                     # priority : 1 * (2 + 3)
  check(9, '(1 + 2) * 3')
  // | "[" exp* "]"                    # array    : [] [1 2]
  check([], '[]')
  check([1, 2], '[1 2]')
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
    puts('// Embedded JavaScript')
    puts(embeddedJs)
    puts()
    puts('// Compiled Moa source code')
    puts(compile(moa).js)
  }
}

main()
