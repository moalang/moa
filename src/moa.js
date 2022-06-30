'use strict'

Error.stackTraceLimit = 100

function Token(code, type) {
  this.code = code
  this.type = type
}
function Type(params) {
  this.name = params.name || ''
  this.types = params.types || []
  this.props = params.props || {}
  this.targs = params.targs || []
  this.dynamic = params.dynamic || false
  this.instance = params.instance || undefined
}
Type.prototype.baseName = function() {
  const show = a => a.length ? `(${a.map(t => t.baseName()).join(' ')})` : ''
  return this.instance ? this.instance.baseName() : this.name
}
Type.prototype.toString = function() {
  const show = a => a.length ? `(${a.map(t => t.toString()).join(' ')})` : ''
  return this.instance ? this.instance.toString() : this.name + show(this.types) + show(this.targs)
}
Type.prototype.pretty = function() {
  const o = {}
  return this.toString().replace(/\d+/g, t => o[t] ||= Object.keys(o).length + 1)
}
Token.prototype.toString = Token.prototype.valueOf = function() { return this.code }
'startsWith match replace'.split(' ').map(m => Token.prototype[m] = function(...a) { return this.code[m](...a) } )

const showType = o =>
  Array.isArray(o) && o.length === 1 ? showType(o[0]) :
  Array.isArray(o) ? `(${o.map(showType).join(' ')})` :
  o.type || o.code
const fs = require('fs')
const str = o => Array.isArray(o) ? (o.length === 1 ? str(o[0]) : '(' + o.map(str).join(' ') + ')') :
  typeof o === 'string' ? o :
  typeof o === 'object' && o.constructor === Token ? o.toString() :
  typeof o === 'object' && o.constructor === Type ? o.toString() :
  JSON.stringify(o)
const put = (...a) => process.stdout.write(str(a))
const puts = (...a) => { console.log(...a.map(str)); return a[a.length - 1] }

const isOp1 = t => t == '!'
const isOp2 = t => t && t.toString().match(/^[+\-*%/=><|&^]+$/) || t == '__pair'
const isAssign = t => '+= -= *= /= %= ='.split(' ').includes(t.code)

const compile = (source, hints) => {
  const tokens = tokenize(source)
  const nodes = infer(parse(tokens), hints)
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
      if (token === ':') {
        if (nesting >= 1) {
          return '__pair'
        } else if (pred && !pred.includes('\n')) {
          return '__line'
        }
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
        a.push(unnest(many(exp)))
      } else if (token.startsWith('__')) {
        break
      } else {
        a.push(f(token))
      }
    }
    return g ? g(a) : a
  }
  const unnest = a => a.length === 1 ? unnest(a[0]) : a
  const line = () => unnest(many(exp, '__eol'))
  const exp = () => glue(bottom())
  const glue = lhs => {
    const pred = tokens[pos++]
    if (isOp1(lhs)) {
      --pos
      return [lhs, exp()]
    } else if (isOp2(pred)) {
      return [pred, lhs, exp()]
    } else if (pred == '.') {
      const rhs = exp()
      if (Array.isArray(rhs) && isOp1(rhs[0]) || isOp2(rhs[0])) {
        return [rhs[0], [pred, lhs, rhs[1]], rhs[2]]
      } else {
        return [pred, lhs, rhs]
      }
    } else if (pred == '__call') {
      return glue(many(exp, ')', a => [new Token('__call'), lhs, ...a]))
    } else if (pred == '__at') {
      return glue([pred, lhs, unnest(many(exp, ']'))])
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
      return many(exp, '}', a => [new Token('__dict'), ...a])
    } else if (token == '(') {
      return unnest(many(exp, ')'))
    } else if (token == '[') {
      return many(exp, ']', a => [new Token('__array'), ...a])
    } else if (token == ':') {
      return many(line, '__eob', a => [new Token('__block'), ...a])
    } else {
      throw Error(`Unexpected token "${token}"`)
    }
  }
  return many(line)
}
const infer = (nodes, hints) => {
  let tvarSequence = 0
  const tenv = {}
  const tvar = () => new Type({name: (++tvarSequence).toString(), dynamic: true})
  const tlambda = (...types) => types.length === 1 ? types[0] : new Type({types})
  const ttype = (name, props, ...targs) => {
    if (targs.length === 0 && name && name in tenv) {
      return tenv[name]
    }
    return tenv[name] =  new Type({name, props, targs})
  }
  const v1 = tvar()
  const tnil = ttype('nil')
  const tint = ttype('int')
  const tbool = ttype('bool')
  const tstring = ttype('string', { size: tint })
  const tset = t => ttype('set', { size: tint }, t)
  const tarray = t => ttype('array', { size: tint, set: tset(t) }, t)
  const tdict = (k, v) => ttype('dictionary', { keys: tarray(k), values: tarray(v) }, k, v)
  const ttime = ttype('time', { year: tint, month: tint, day: tint, hour: tint, minute: tint, second: tint })
  const ttest = ttype('__test', { eq: tlambda(v1, v1, tnil) })

  const prune = t => (t.dynamic && t.instance) ? t.instance = prune(t.instance) : t
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
  const analyze = (node, env, nonGeneric) => node.type = _assert(_analyze(node, env, nonGeneric), node)
  const _assert = (type, node) => type || (() => {throw Error(`Type can not be infered ${node}`)})()
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
      } else if (head == 'adt') {
        const name = tail[0].code
        const f = o => Array.isArray(o) ? [o[0].code, ttype(o[1].code)] : [o.code, tnil]
        const kvs = tail[1].slice(1).map(f)
        const adt = ttype(name, () => Object.fromEntries(kvs))
        kvs.map(([name, alias]) => env[name] = tlambda(alias, adt))
        return env[name] = adt
      } else if (head == 'match') {
        const target = analyze(tail[0], env, nonGeneric)
        const ret = tvar()
        for (let cond of tail[1].slice(1)) {
          if (cond.length === 3) {
            const tag = cond[0].code
            const alias = cond[1].code
            const exp = cond[2]
            const newEnv = Object.assign({}, env)
            newEnv[alias] = env[tag].types[0]
            unify(ret, analyze(exp, newEnv, nonGeneric), exp)
          } else if (cond.length === 2) {
            unify(ret, analyze(cond[1], env, nonGeneric), cond[1])
          } else {
            throw Error('Unknown condition of match')
          }
        }
        return ret
      } else if (head == 'return') {
        return tail[0] ? analyze(tail[0], env, nonGeneric) : tnil
      } else if (head == 'case') {
        for (let i=0; i<tail.length - 1; i+=2) {
          unify(tbool, analyze(tail[i], env, nonGeneric), tail[i])
        }
        const ret = tvar()
        for (let i=1; i<tail.length; i+=2) {
          unify(ret, analyze(tail[i], env, nonGeneric), tail[i])
        }
        unify(ret, analyze(tail[tail.length - 1], env, nonGeneric), tail[tail.length - 1])
        return ret
      } else if (head == 'test') {
        const newEnv = Object.assign({[tail[0].code]: ttest}, env)
        return analyze(tail.slice(1), newEnv, nonGeneric)
      } else if (head == '__call') {
        if (tail[0] == 'fail') {
          analyze(tail.slice(1), env, nonGeneric)
          return tvar()
        } else {
          return analyze(tail, env, nonGeneric)
        }
      } else if (head == '__block') {
        if (tail.length === 1 && tail[0].length === 0) {
          return tnil
        }
        predict(env, tail)
        const ret = tail.map(line => analyze(line, env, nonGeneric)).slice(-1)[0]
        const f = node => node[0] == 'if' ? g(node[2]) : ''
        const g = node => node[0] == 'return' ? unify(ret, node[1].type, node[1]) : f(node)
        tail.map(f)
        return ret
      } else if (head == '__array') {
        const ta = tarray(tvar())
        tail.map(x => unify(ta.targs[0], analyze(x, env, nonGeneric), x))
        return ta
      } else if (head == '__at') {
        const ary = analyze(tail[0], env, nonGeneric)
        const index = analyze(tail[1], env, nonGeneric)
        unify(tint, index, tail)
        return ary.targs[0]
      } else if (head == '__dict') {
        const td = tdict(tvar(), tvar())
        for (const x of tail) {
          if (Array.isArray(x)) {
            unify(td.targs[0], analyze(x[1], env, nonGeneric))
            unify(td.targs[1], analyze(x[2], env, nonGeneric))
          } else {
            unify(td.targs[0], tstring)
            unify(td.targs[1], analyze(x, env, nonGeneric))
          }
        }
        return td
      } else if (head == '=>') {
        const newEnv = Object.assign({}, env)
        const targs = tail[0].code ? tail[0].code.split(',').map((s,i) => new Token(s, tvar())) : []
        targs.map(arg => newEnv[arg.code] = arg.type)
        return tlambda(...targs.map(t => t.type), analyze(tail[1], newEnv, nonGeneric.concat(targs.map(t => t.type.name))))
      } else if (head == '.') {
        const type =  analyze(tail[0], env, nonGeneric)
        const name = tail[1].code
        return type.props[name] || (() => {throw Error(`'${name}' not found in '${Object.keys(type.props)}' of ${type}`)})()
      } else if (head.array && tail.length) {
        const t = analyze(head, env, nonGeneric)
        if (tail.length !== 1) {
          throw new Error(`Array calling should take 1 argument but gave ${tail.length}`)
        }
        unify(tint, analyze(tail[0], env, nonGeneric), tail[0])
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
      return tarray(tvar())
    } else if (node == '__dict') {
      return ttype('', () => ({}))
    } else {
      const v = node.code
      if (v.match(/^[0-9]/)) {
        return tint
      } else if (v.match(/^["`]/)) {
        return tstring
      } else if (v === 'return') {
        return tnil // return without value
      } else if (v in env) {
        return fresh(env[v], nonGeneric)
      } else {
        throw Error(`Not found ${v} in ${Object.keys(env)}`)
      }
    }
  }
  const topEnv = Object.assign({
    __cache: {},
    'true': tbool,
    'false': tbool,
    '<': tlambda(tint, tint, tbool),
    '!': tlambda(tbool, tbool),
    '&&': tlambda(tbool, tbool, tbool),
    'if': tlambda(tbool, v1, tnil),
    'case': tlambda(tbool, v1, v1, v1),
    'time': tlambda(tint, tint, tint, tint, tint, tint, ttime),
  }, hints || {})
  '+ - * / % += -= *= /= %='.split(' ').map(op => topEnv[op] = tlambda(tint, tint, tint))
  const defs = 'fn struct'.split(' ')
  const predict = (env, nodes) => nodes.map(x => Array.isArray(x) && defs.includes(x[0].code) ? env[x[1]] = tvar() : null)
  predict(topEnv, nodes)
  nodes.map(node => analyze(node, topEnv, []))
  nodes.hints = topEnv
  return nodes
}
const generate = nodes => {
  const embedded = {
    'array_size': (o, name) => `${gen(o)}.length`,
    'array_set': (o, name) => `((d,a) => a.flatMap(x => x in d ? [] : [d[x]=x]))({}, ${gen(o)})`,
    'string_size': (o, name) => `${gen(o)}.length`
  }
  const gen = o => Array.isArray(o) ? apply(o) :
    o == '__array' ? '[]' :
    o == '__dict' ? '({})' :
    o.startsWith('`') ? template(o) : o
  const template = s => s.replace(/\$[A-Za-z0-9_.]+/g, x => '${' + x.slice(1) + '}')
  const _case = a => a.length === 0 ? (() => {throw Error('Invalid case expression')})() :
    a.length === 1 ? a[0] :
    `${a[0]} ? ${a[1]} : ` + _case(a.slice(2))
  const isExp = code => !/^(?:if|for|return)/.test(code)
  const statement = a => a.map((v, i) => a.length - 1 === i && isExp(v) ? 'return ' + v : v)
  const apply = node => {
    if (node.length === 0) {
      return 'undefined'
    }
    const [head, ...tail] = node
    const args = tail.map(gen)
    // internal marks
    if (head == '__call') {
      if (tail[0] == 'fail') {
        return `(() => { throw Error(${gen(tail[1])}) })()`
      } else {
        return args[0] + `(${args.slice(1)})`
      }
    } else if (head == '__block') {
      return args.length === 1 && isExp(args[0]) ? args[0] : '{\n  ' + statement(args).join('\n  ') + '\n}'
    } else if (head == '__array') {
      return `[${args.join(', ')}]`
    } else if (head == '__dict') {
      const kvs = tail.map(o => Array.isArray(o) ? `${o[1]}:${o[2]}` : o).join(',')
      return `({${kvs}})`
    } else if (head == '__at') {
      return args[0] + `[${args[1]}]`

    // operators
    } else if (head == '.') {
      const key = tail[0].type.baseName() + '_' + args[1]
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
      } else if (head == '/') {
        return `(((l,r) => r === 0 ? (() => { throw Error('Zero division error') })() : l / r)(${lhs},${rhs}))`
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
      const f = o => Array.isArray(o) ?
        `const ${o[0]} = __val => ({__tag:'${o[0]}',__val})` :
        `const ${o} = {__tag:'${o}'}`
      return tail[1].slice(1).map(f).join('\n')
    } else if (head == 'if') {
      return `if (${args[0]}) { ${args[1]} }`
    } else if (head == 'case') {
      return _case(args)
    } else if (head == 'match') {
      const error = '(()=>{throw Error(`Unmatch error: "${__.__tag}"`)})()'
      const f = a => a.length === 3 ? `__.__tag === '${a[0]}' ? (${gen(a[1])} => ${gen(a[2])})(__.__val) : ` :
        a.length === 2 ? `__.__tag === '${a[0]}' ? ${gen(a[1])} : ` :
        (() => {throw Error(`Unexpected condition of match ${a}`)})()
      const match = tail[1].slice(1).map(f).join('\n  ') + error
      return `(__ => ${match})(${args[0]})`
    } else if (head == 'test') {
      return `const __test = ${tail[0]} => ` + gen(tail[1])

    // general
    } else {
      return `${gen(head)}(${args})`
    }
  }
  const js = 'const time = (year,month,day,hour,minute,second) => ({year,month,day,hour,minute,second})'
  return js + '\n' + nodes.map(gen).join(';\n')
}


const testAll = () => {
  const test = (convert, expect, exp, ...defs) => {
    const source = (defs || []).concat(`fn main:\n  ${exp.replace(/\n/g, "\n  ")}`).join('\n')
    const tokens = tokenize(source)
    let nodes = parse(tokens)
    let js = ''
    let ret
    let stack
    try {
      const __tester = {
        eq: (a,b) => {
          if (str(a) !== str(b)) {
            throw Error(`Test failed: expect(${a}) but actual(${b})`)
          }
        }
      }
      nodes = infer(nodes)
      js = generate(nodes)
      ret = eval(js + "\ntypeof __test !== 'undefined' && __test(__tester)\nmain()")
    } catch(e) {
      ret = e.message
      stack = e.stack
    }
    const actual = convert(ret, nodes.slice(-1)[0])
    if (actual === expect) {
      put('.')
    } else {
      puts('Failed')
      puts('source:', source)
      puts('js    :', js)
      puts('nodes :', nodes)
      puts('types :', showType(nodes))
      puts('tokens:', tokens)
      puts('expect:', expect)
      puts('actual:', actual)
      puts('stack :', stack)
      process.exit(1)
    }
  }
  const reject = (exp, ...defs) => test((ret) => /^type miss match/.test(ret) ? ret.slice(0, 15) : ret, 'type miss match', exp, ...defs)
  const inf = (expect, exp, ...defs) => test((ret, main) => main.type ? main.type.pretty() : ret, expect, exp, ...defs)
  const check = (expect, exp, ...defs) => test((ret) => str(ret), str(expect), exp, ...defs)
  const error = (expect, exp, ...defs) => test((ret) => ret, str(expect), exp, ...defs)

  // -- Tests for type inference
  // primitives
  inf('int', '1')
  inf('bool', 'true')
  inf('bool', 'false')
  // exp
  inf('int', '+ 1 1')
  inf('bool', '< 1 1')
  // branch
  inf('int', 'case true 1 2')
  inf('bool', 'case true true true')
  inf('nil', 'if true: return')
  inf('int', 'if true: return 1\n2')
  // value
  inf('int', 'fn value 1')
  // lambda
  inf('int', '() => 1')
  inf('(int int)', 'x => x + 1')
  // simple function
  inf('(int int)', 'fn inc a: a + 1')
  inf('(int int int)', 'fn add a b: a + b')
  // generic function
  inf('(1 1)', 'fn f a a')
  inf('(1 2 1)', 'fn f a b a')
  inf('(1 2 2)', 'fn f a b b')
  inf('int', 'fn f a a\nf 1')
  inf('bool', 'fn f a a\nf 1\nf true')
  // dict
  inf('dictionary(string int)', '{"a":1}')
  // string
  inf('int', '"hi".size')
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
  // | "{" id* (id "=" exp)* "}"       # dictionary: {}, {one "two":2}
  check({}, '{}')
  check({one: 1, two: 2}, '{one "two":2}', 'let one 1')
  // | '"' [^"]* '"'                   # string   : "hi"
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

  // qw(let var fn struct adt if return case match fail test)
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
  check(1, '1', 'test t: t.eq 1 1')
  check('Test failed: expect(1) but actual(0)', '1', 'test t: t.eq 1 0')
  check('2001/2/3 4:5:6', '`$now.year/$now.month/$now.day $now.hour:$now.minute:$now.second`', 'let now time(2001 2 3 4 5 6)')

  error('hi', 'fail("hi")')
  error('Zero division error', '1/0')

  // priority of operators
  check(3, '"hi".size + 1')
  check(3, '1 + "hi".size')

  puts('ok')
}


const interactive = async () => {
  puts('Moa 0.0.1 May 23 2022, 21:26')
  const hints = {}
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
    let {js, nodes} = compile(cmd, hints)
    Object.assign(hints, nodes.hints)
    js = js.replace(/^let |const /g, 'global.')
    try {
      puts(eval(js))
    } catch (e) {
      puts(e.stack)
    }
    puts('js:', js)
    puts('node:', str(nodes))
    puts('type:', showType(nodes))
    rl.prompt()
  }).on('close', () => {
    puts('Bye👋')
  })
}


const main = () => {
  const [cmd, ...args] = process.argv.slice(2)
  if (cmd === 'selfcheck') {
    testAll()
  } else if (cmd === 'compile') {
    const moa = args.map(path => fs.readFileSync(path, 'utf-8')).join('\n\n')
    puts(compile(moa).js)
  } else {
    interactive()
  }
}


main()
