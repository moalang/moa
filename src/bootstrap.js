'use strict'

// helpers
const write = (...a) => a.map(o => process.stdout.write(o.toString()))
const puts = (...a) => console.log(...a)
const dump = o => console.dir(o, {depth: null})
const eq = (a, b) => a === b || str(a) === str(b)
const str = o => JSON.stringify(o)
const err = (msg, o) => (puts('Error:', msg), dump(o), new Error(msg))
const newType = (__type, keys, vals) => keys.reduce((acc,k,i) => (acc[k]=vals[i], acc), {__type})
const test = (expect, actual, o) => {
  if (eq(expect, actual)) {
    write('.')
  } else {
    puts('Failed')
    puts('expect:', expect)
    puts('actual:', actual)
    dump(o)
  }
}
const isPrimitive = o => {
  const t = typeof o
  return t === 'number' || t === 'string' || (t === 'object' && o.constructor === Array) || ['some', 'error'].includes(o.__type)
}
const typeName = o => o.__type || typeof o
const functions = {
  trace: o => (dump(o),o),
  guard: cond => cond ? ({__type: 'some', content: true}) : ({__type: 'error', message: 'guard'}),
  some: content => ({__type: 'some', content}),
  error: message => ({__type: 'error', message}),
}
const methods = {
  object: { // array
    size: a => a.length,
    map: (a,f) => a.map(f),
    concat: (a,b) => a.concat(b),
  },
  string: {
    length: s => s.length,
    at: (s,n) => n < s.length ? functions.some(s[n]) : functions.error('out of index'),
    to_int: s => s.match(/^[0-9]+$/) ? functions.some(parseInt(s)) : functions.error('not a number'),
  },
  some: {
    map: (s,f) => (s.content = f(s.content), s),
    alt: (s,_) => s
  },
  error: {
    map: (e,_) => e,
    alt: (_,a) => ({__type: 'some', content: a}),
  },
}

// main process
const tokenize = src => {
  const len = src.length
  let left = src
  let indent = 0
  let line = 1
  const token = (tag, token, f) => token ? {token, [tag]: f ? f(token) : token} : false
  const match = (tag, r, f) => {
    const m = left.match(r)
    return token(tag, m ? m[0] : m, f)
  }
  const any = (tag, strings, f) => token(tag, strings.find(s => s === left.slice(0, s.length)), f)
  const op2s = '<- -> := += -= *= /= == != => <= >= && || + - * / < >'.split(' ')
  const syms = '| : = ( ) [ ]'.split(' ')
  const tokens = []
  while (left) {
    const o = any('op2', op2s) ||
      any('value', ['true', 'false'], s => s === 'true') ||
      match('id', /^(\[\])*[a-zA-Z_]+[a-zA-Z0-9_]*/) ||
      any('sym', syms) ||
      any('dot', ['.']) ||
      match('value', /^[0-9]+/, parseInt) ||
      match('value', /^(["'`])[^\1]*\1/, s => s.slice(1, -1)) ||
      match('comment', /^#.+/) ||
      match('spaces', /^[ \t\n]+/)
    if (!o) { throw err('unknown token', {src,left,tokens}) }
    left = left.slice(o.token.length)
    if (o.spaces) {
      const br = (o.token.match(/\n/g) || []).length
      line += br
      if (br) {
        indent = o.token.split('\n').slice(-1)[0].length
      }
    }
    o.line = line
    o.indent = indent
    o.index = len - left.length - o.token.length
    if (o.comment || o.spaces) { continue }
    tokens.push(o)
  }
  return tokens
}
const parse = tokens => {
  const len = tokens.length
  let pos = 0
  const consume = () => tokens[pos++]
  const check = (...fs) => fs.every((f,i) => (pos+i < len && f(tokens[pos+i])))
  const until = (f, g) => {
    const a = []
    while (pos < len && f(tokens[pos])) {
      a.push(g())
    }
    return a
  }
  const drop = (token, value) => {
    const node = consume()
    if (node.token !== token) { throw err('Unexpected token', {node, value}) }
    return value
  }
  const operator = lhs => {
    if (lhs.sym === '[') {
      return operator({ array: drop(']', until(t => t.sym !== ']', parse_unit)) })
    }
    if (lhs.sym === '(') {
      return drop(')', parse_unit())
    }
    const next = consume()
    if (!next) { return lhs }
    if (next.op2) {
      const o1 = next.op2
      if (tokens[pos].sym === '(') {
        return operator({ op2: o1, lhs, rhs: parse_unit() })
      } else {
        const rhs = parse_unit()
        if (rhs.op2) {
          const o2 = rhs.op2
          if ('*/'.includes(o1) && '+-'.includes(o2)) {
            return operator({ op2: o2, lhs: { op2: o1, lhs, rhs: rhs.lhs }, rhs: rhs.rhs })
          } else {
            return operator({ op2: o1, lhs, rhs })
          }
        } else {
          return operator({op2: next.token, lhs, rhs})
        }
      }
    } else if (next.sym === '(') {
      if (lhs.index + lhs.token.length  === next.index) {
        return operator({ call: lhs.token, args: drop(')', until(t => t.sym !== ')', parse_unit)) })
      } else {
        throw err('unsupported yet operator', {next})
      }
    } else if (next.dot) {
      const method = consume()
      const args = check(t => t.token === '(' && method.index + method.token.length === t.index) ?
        (consume(), drop(')', until(t => t.sym !== ')', parse_unit))) : []
      return operator({self: lhs, method, args})
    } else {
      pos--
      return lhs
    }
  }
  const parse_unit = () => operator(consume())
  const parse_line = () => check(t => t.id, t => (t.id && t.line === tokens[pos].line) || '=:|'.includes(t.sym)) ? parse_define() : parse_unit()
  const parse_define = () => {
    const first = consume()
    const name = first.token
    const args = until(t => t.id && t.line === first.line, consume).map(t => t.token)
    const sym = consume()
    switch (sym.token) {
      case '=': return {name, args, body: parse_body(first)}
      case ':': return {name, args, struct: parse_struct(2)}
      case '|': return {name, args, adt: parse_adt()}
      default: throw err('Unknown token', {sym,pos,around:tokens.slice(pos,pos+3)})
    }
  }
  const parse_body = base => check(t => t.line === base.line) ? parse_unit() : parse_flow(base)
  const parse_flow = base => ({flow: until(t => t.indent > base.indent, parse_line)})
  const parse_struct = indent => {
    const fields = []
    let line = 0
    until(t => t.indent === indent, () => {
      const t = consume()
      if (line !== t.line) {
        fields.push(t.id)
        line = t.line
      }
    })
    return fields
  }
  const parse_adt = () => {
    const fields = []
    let line = 0
    until(t => t.indent === 2, () => {
      const t = consume()
      if (line !== t.line) {
        if (check(t => t.token === ':')) {
          drop(':')
          fields.push([t.id, ...parse_struct(4)])
        } else {
          fields.push([t.id])
        }
        line = t.line
      }
    })
    return fields
  }

  const nodes = []
  while (pos < len) {
    nodes.push(parse_define())
  }
  return nodes
}
const evaluate = nodes => {
  function environment(parent) {
    const d = {}
    this.get = key => (key in d ? d[key] : parent ? parent.get(key) : (() => { throw err('id not found', {key,keys:this.keys()}) })())
    this.put = (key,value) => d[key] = value
    this.local = (keys,values) => {
      const env = new environment(this)
      keys.map((key, i) => env.put(key, values[i]))
      return env
    }
    this.keys = () => Object.keys(d).concat(parent ? parent.keys() : [])
  }
  const root = new environment()
  const define = node => {
    if (node.adt) {
      for (const [name,...adt] of node.adt) {
        if (adt.length) {
          root.put(name, {name, adt})
        } else {
          root.put(name, {value: {__type: name}})
        }
      }
    } else {
      root.put(node.name, node)
    }
  }
  nodes.map(define)
  const unwrap = (o, env) => {
    o = execute(o, env)
    if (o.flow) {
      for (const line of o.flow) {
        if (line.name) {
          define(line)
        } else {
          o = unwrap(line, o.env || env)
          if (o.__type === 'error') {
            return o
          }
        }
      }
    }
    return o.__type === 'some' ? o.content : o
  }
  const execute = (node, env) => {
    if (isPrimitive(node)) { return node }
    if ('value' in node) { return node.value }
    if (node.array) { return node.array.map(x => execute(x, env)) }
    if (node.body) { return execute(node.body, env) }
    if (node.id) { return node.id in functions ? functions[node.id] : execute(env.get(node.id), env) }
    if (node.call) {
      if (node.call === 'if') {
        for (let i=0; i<node.args.length-1; i+=2) {
          if (execute(node.args[i], env)) {
            return execute(node.args[i+1], env)
          }
        }
        return execute(node.args[node.args.length - 1], env)
      } else if (node.call === 'match') {
        const target = execute(node.args[0], env)
        for (const arg of node.args.slice(1)) {
          if (arg.lhs.id) {
            if (target.__type === arg.lhs.id) {
              return execute(arg.rhs, env)
            }
          } else if (arg.lhs.call) {
            if (target.__type === arg.lhs.call) {
              const keys = arg.lhs.args.map(a => a.id)
              const values = Object.values(target).slice(1)
              return execute(arg.rhs, env.local(keys, values))
            }
          } else {
            if (execute(arg.lhs, env) === target) {
              return execute(arg.rhs, env)
            }
          }
        }
        throw err('Switch does not match', {target, node})
      } else if (node.call in functions) {
        const f = functions[node.call]
        return f.apply(null, node.args.map(arg => execute(arg, env)))
      } else {
        const def = env.get(node.call)
        const argv = node.args.map(arg => execute(arg, env))
        if (def.body) {
          return execute(def.body, env.local(def.args, argv))
        } else if (typeof def === 'function') {
          return def.apply(argv)
        } else if (def.struct) {
          return newType(def.name, def.struct, argv)
        } else if (def.adt) {
          return newType(def.name, def.adt, argv)
        } else {
          throw err('Unknown apply', {node, def: def.toString(), keys: env.dump()})
        }
      }
    }
    if (node.method) {
      let obj = execute(node.self, env)
      const name = node.method.id
      const args = node.args.map(a => execute(a, env))
      const type = typeName(obj)
      if (!(type in methods)) { throw err('type not found', {node,obj,type,name}) }
      if (!(name in methods[type])) { throw err('method not found', {node,obj,type,name}) }
      return methods[type][name](obj, ...args)
    }
    if (node.op2) {
      if (node.op2 === '=>') {
        return arg => execute(node.rhs, env.local([node.lhs.id], [arg]))
      } else if (node.op2 === '<-') {
        const o = unwrap(node.rhs, env)
        const v = o.__type === 'some' ? o.content : o
        return env.put(node.lhs.id, v)
      } else {
        const l = execute(node.lhs, env)
        const r = execute(node.rhs, env)
        switch (node.op2) {
          case '+': return l + r
          case '-': return l - r
          case '*': return l * r
          case '/': return l / r
          case '+=': return env.put(node.lhs.id, l + r)
          case '-=': return env.put(node.lhs.id, l - r)
          case '*=': return env.put(node.lhs.id, l * r)
          case '/=': return env.put(node.lhs.id, l / r)
          default: throw err('Unknown operator', node)
        }
      }
    }
    if (node.flow) {
      node.env = env
      return node
    }
    throw err('Unknown node', {node, dump: env.dump()})
  }
  return unwrap(root.get('main').body, root)
}
const run = src => {
  const tokens = tokenize(src)
  const nodes = parse(tokens)
  const value = evaluate(nodes)
  return {tokens, nodes, value}
}

// tests
const testTokenize = () => {
  function t(expect, src) {
    test(expect, tokenize(src), {src})
  }
  t([{ token: 'true', value: true, line: 1, indent: 0, index: 0 }], 'true')
  t([{ token: '"hi"', value: 'hi', line: 1, indent: 0, index: 0 }], '"hi"')
  t([
    { token: 'a', id: 'a', line: 1, indent: 0, index: 0 },
    { token: '=', sym: '=', line: 1, indent: 0, index: 2 },
    { token: '1', value: 1, line: 1, indent: 0, index: 4 }
  ], 'a = 1')
  t([
    { token: 'struct', id: 'struct', line: 1, indent: 0, index: 0 },
    { token: 'a', id: 'a', line: 1, indent: 0, index: 7 },
    { token: ':', sym: ':', line: 1, indent: 0, index: 8 },
    { token: 'value', id: 'value', line: 2, indent: 2, index: 12 },
    { token: 'a', id: 'a', line: 2, indent: 2, index: 18 },
    { token: 'method', id: 'method', line: 3, indent: 2, index: 22 },
    { token: 'offset', id: 'offset', line: 3, indent: 2, index: 29 },
    { token: ':', sym: ':', line: 3, indent: 2, index: 35 },
    { token: 'value', id: 'value', line: 3, indent: 2, index: 37 },
    { token: '+', op2: '+', line: 3, indent: 2, index: 43 },
    { token: 'offset', id: 'offset', line: 3, indent: 2, index: 45 }
  ], 'struct a:\n  value a\n  method offset: value + offset')
  t([
    { token: 'a', id: 'a', line: 1, indent: 0, index: 0 },
    { token: '=', sym: '=', line: 1, indent: 0, index: 2 },
    { token: '[]int', id: '[]int', line: 1, indent: 0, index: 4 }
  ], 'a = []int')
  t([], '# comment')
}
const testParse = () => {
  const show = node => node.value ? node.value.toString() :
      node.id ? node.id :
      node.body ? node.name + node.args.map(x => ' ' + x).join('') + ' = ' + show(node.body) :
      node.call ? node.call + '(' + node.args.map(show).join(' ') + ')' :
      node.op2 ? '(' + show(node.lhs) + ' ' + node.op2 + ' ' + show(node.rhs) + ')' :
      node.struct ? [node.name, ...node.args].join(' ') + ': ' + node.struct.join(' ') :
      node.adt ? [node.name, ...node.args].join(' ') + '| ' + node.adt.map(e => e.join(' ')).join(', ') :
      node.flow ? node.flow.map(show).join(' ; ') :
      node.self ? show(node.self) + '.' + node.method.id + (node.args.length ? '(' + node.args.map(show).join(' ') + ')' : '')  :
      (() => { throw err('Unknown', {node}) })()
  const t = (expect, src) => {
    const tokens = tokenize(src)
    const nodes = parse(tokens)
    test(expect, nodes.map(show).join('\n'), {src,nodes})
  }
  t('f = 1', 'f = 1')
  t('f = (1 + (2 + (3 * (4 * 5))))', 'f = 1 + 2 + 3 * 4 * 5')
  t('f a = (a + 1)', 'f a = a + 1')
  t('f a = (a + (1 * 2))', 'f a = a + 1 * 2')
  t('f a = ((a * 1) + 2)', 'f a = a * 1 + 2')
  t('f a = a\ng = f((1 + 2))\nh = g()', 'f a = a\ng = f(1 + 2)\nh = g()')
  t('f = (g(1) + h(2))', 'f = g(1) + h(2)')
  t('f = (1 * (2 + 3))', 'f = 1*(2+3)')
  t('struct: field1', 'struct:\n  field1 type1')
  t('struct t1 t2: field1 field2', 'struct t1 t2:\n  field1 t1\n  field2 t2')
  t('adt| tag', 'adt|\n  tag')
  t('adt a| tag a', 'adt a|\n  tag:\n    a int')
  t('adt a b| tag1, tag2 f1 f2', 'adt a b|\n  tag1\n  tag2:\n    f1 a\n    f2 b')
  t('f = (a <- 1) ; a', 'f =\n  a <- 1\n  a')
  t('f = (a <- 1) ; (a := 2) ; a', 'f =\n  a <- 1\n  a := 2\n  a')
  t('f a = a\ng = f(1)', 'f a =\n  a\ng = f(1)')
  t('f a = a.b.c', 'f a = a.b.c')
  t('f a = a.b(1).c(2)', 'f a = a.b(1).c(2)')
}
const testEvaluate = () => {
  const t = (expect, exp, ...defs) => {
    defs.push('main = ' + exp)
    const src = defs.join('\n')
    test(expect, run(src).value, src)
  }

  // basic value
  t(1, '1')
  t('hi', '"hi"')
  t(true, 'true')
  t([], '[]')

  // expression
  t(7, '1+2*3')
  t(5, '1*2+3')

  // function
  t(1, 'f', 'f = 1')
  t(2, 'f(1)', 'f a = a + 1')
  t(3, 'f(1) + g(1)', 'f a = a', 'g a = a + f(a)')
  t(3, 'add(1 2)', 'add a b = a + b')

  // struct
  t({__type: 'wrap', value: 1}, 'wrap(1)', 'wrap:\n  value int')
  t({__type: 'vector2', x: 1, y: 2}, 'vector2(1 2)', 'vector2 a:\n  x a\n  y a')
  t({__type: 'list', values: [1]}, 'list([1])', 'list:\n  values []int')

  // adt
  t({__type: 'just', value: 1}, 'just(1)', 'may a|\n  just:\n    value a\n  none')
  t({__type: 'none'}, 'none', 'may a|\n  just:\n    value a\n  none')

  // control flow
  t(1, 'if(true 1 not_found)')
  t(2, 'if(false not_found 2)')
  t(3, 'if(false not_found true 3 not_found)')
  t(4, 'if(false not_found false not_found 4)')

  // pattern match
  t(10, 'match(1 1->10 2->20)')
  t(20, 'match(2 1->10 2->20)')
  t(2, 'match(none just(a)->a none->2)', 'may a|\n  just:\n    value a\n  none')
  t(1, 'match(just(1) just(a)->a none->2)', 'may a|\n  just:\n    value a\n  none')

  // optinal
  t(1, 'some(1)')
  t({__type: 'error', message: 'hi'}, 'error("hi")')
  t(3, 'some(1).map(x => x + 2)')
  t({__type: 'error', message: 'hi'}, 'error("hi").map(x => x + 2)')
  t(1, 'some(1).alt(2)')
  t(2, 'error("hi").alt(2)')

  // flow
  t(2, '\n  a <- 1\n  a += 1\n  a')
  t({__type: 'error', message: 'hi'}, '\n  a <- error("hi")\n  1')
  t({__type: 'error', message: 'guard'}, '\n  guard(false)\n  1')

  // string methods
  t(5, '"hello".length')
  t(12, '"12".to_int')
  t({__type: 'error', message: 'not a number'}, '"1a".to_int')

  // array methods
  t('i', '"hi".at(1)')
  t(0, '[].size')
  t([1, 4], '[0 1+2].map(v => v + 1)')
}
const testMoa = base => {
  const baseNodes = parse(tokenize(base))

  const t = (expect, src) => {
    const nodes = baseNodes.concat(parse(tokenize('main = compile_js(' + JSON.stringify(src) + ')')))
    test(expect, evaluate(nodes), src)
  }

  t(1, '1')
}

// main
const main = () => {
  testTokenize()
  testParse()
  testEvaluate()
  testMoa(require('fs').readFileSync('moa.moa', {encoding: 'utf-8'}))
  puts('ok')
}
main()
