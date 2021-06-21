'use strict'

// helpers
function write(...a) { a.map(o => process.stdout.write(o.toString())) }
function puts(...a) { console.log(...a) }
function dump(o) { console.dir(o, {depth: null}) }
function eq(a, b) { return a === b || str(a) === str(b) }
function str(o) { return JSON.stringify(o) }
function err(msg, o) { dump(o); return new Error(msg) }
function newType(__type, keys, vals) { return keys.reduce((acc,k,i) => (acc[k]=vals[i], acc), {__type}) }
function test(expect, actual, o) {
  if (eq(expect, actual)) {
    write('.')
  } else {
    puts('Failed')
    puts('expect:', expect)
    puts('actual:', actual)
    dump(o)
  }
}
function isPrimitive(o) {
  const t = typeof o
  return t === 'number' || t === 'string' || (t === 'object' && o.constructor === Array)
}
function typeName(o) {
  return o.__type || typeof o
}
const functions = {
  some: content => ({__type: 'some', content}),
  error: message => ({__type: 'error', message}),
}
const methods = {
  object: { // array
    size: a => a.length,
    map: (a,f) => a.map(f),
  },
  string: {
    length: s => s.length,
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
function tokenize(src) {
  const op2s = ':= += -= *= /= != =>'.split(' ')
  let indent = 0
  let line = 1
  return [...src.matchAll(/\s+|#.+|(\[\])?[a-zA-Z0-9_]+|&&|\|\||[:+\-*/!<>=]=|->|<-|[|:]|[+\-*\/=><]+|".+?"|[\[\]()]|./g)].map(s => {
    const token = s[0]
    const br = (token.match(/\n/g) || []).length
    line += br
    if (br) { indent = token.split('\n').slice(-1)[0].length }
    const o = {
      token,
      indent,
      line,
      index: s.index,
    }
    if (token[0] === '#') { o.token = '' }
    else if (token === 'true' || token === 'false') { o.value = token === 'true' }
    else if (token.match(/^(\[\])?[a-zA-Z_]/)) { o.id = o.token }
    else if (token.match(/^[0-9]+$/)) { o.value = parseInt(o.token) }
    else if (token.match(/^["'`]/)) { o.value = o.token.slice(1, -1) }
    else if (op2s.includes(token)) { o.op2 = o.token }
    else if (token.match(/^[=:|()\[\]]$/)) { o.sym = o.token }
    else { o.op2 = o.token }
    return o
  }).filter(x => x.token.trim())
}
function parse(tokens) {
  const len = tokens.length
  let pos = 0
  function consume() {
    return tokens[pos++]
  }
  function check(...fs) {
    return fs.every((f,i) => (pos+i < len && f(tokens[pos+i])))
  }
  function unit() {
    return operator(consume())
  }
  function expect(token) {
    return pos < len && tokens[pos].token === token
  }
  function drop(token, value) {
    const node = consume()
    if (node.token !== token) { throw err('Unexpected token', {node, value}) }
    return value
  }
  function operator(lhs) {
    if (lhs.sym === '[') {
      return operator({ array: drop(']', until(t => t.sym !== ']', unit)) })
    }
    if (lhs.sym === '(') {
      return drop(')', unit())
    }
    const next = consume()
    if (!next) { return lhs }
    if (next.op2) {
      const o1 = next.op2
      if (tokens[pos].sym === '(') {
        return operator({ op2: o1, lhs, rhs: unit() })
      } else {
        const rhs = unit()
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
        return operator({ call: lhs.token, args: drop(')', until(t => t.sym !== ')', unit)) })
      } else {
        throw err('unsupported yet operator', {next})
      }
    } else {
      pos--
      return lhs
    }
  }
  function until(f, g) {
    const a = []
    while (pos < len && f(tokens[pos])) {
      a.push(g())
    }
    return a
  }
  function parse_line() {
    return check(t => t.id, t => t.id || '=:|'.includes(t.sym)) ? parse_define() : unit()
  }
  function parse_define() {
    const [name, ...args] = until(t => t.id, consume).map(t => t.token)
    const sym = consume()
    switch (sym.token) {
      case '=': return {name, args, body: parse_body(sym)}
      case ':': return {name, args, struct: parse_struct(2)}
      case '|': return {name, args, adt: parse_adt()}
      default: throw err('Unknown token', {sym,pos,tokens})
    }
  }
  function parse_body(base) {
    return check(t => t.line === base.line) ? unit() : parse_flow(base)
  }
  function parse_flow(base) {
    return {flow: until(t => t.indent > base.indent, parse_line)}
  }
  function parse_struct(indent) {
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
  function parse_adt() {
    const fields = []
    let line = 0
    until(t => t.indent === 2, () => {
      const t = consume()
      if (line !== t.line) {
        if (expect(':')) {
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
function evaluate(nodes) {
  const environment = function(parent) {
    const d = {}
    this.get = key => (key in d ? d[key] : parent ? parent.get(key) : (() => { throw err('not found', {key,d,names:this.dump()}) })())
    this.put = (key,value) => d[key] = value
    this.new = (keys,values) => {
      const env = new environment(this)
      keys.map((key, i) => env.put(key, values[i]))
      return env
    }
    this.dump = () => Object.keys(d).concat(parent ? parent.dump() : [])
  }
  const root = new environment()
  for (const node of nodes) {
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
  function execute(node, env) {
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
              return execute(arg.rhs, env.new(keys, values))
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
          return execute(def.body, env.new(def.args, argv))
        } else if (def.struct) {
          return newType(def.name, def.struct, argv)
        } else if (def.adt) {
          return newType(def.name, def.adt, argv)
        } else {
          throw err('Unknown apply', def)
        }
      }
    }
    if (node.op2) {
      if (node.op2 === '.') {
        const obj = execute(node.lhs, env)
        const name = node.rhs.id || node.rhs.call
        const args = node.rhs.call ? node.rhs.args.map(a => execute(a, env)) : []
        const type = typeName(obj)
        if (!(type in methods)) { throw err('type is not found', {obj,type,name}) }
        if (!(name in methods[type])) { throw err('method is not found', {obj,type,name}) }
        return methods[type][name](obj, ...args)
      } else if (node.op2 === '=>') {
        return arg => execute(node.rhs, env.new([node.lhs.id], [arg]))
      } else if (node.op2 === '<-') {
        return env.put(node.lhs.id, execute(node.rhs, env))
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
      let ret
      for (const line of node.flow) {
        ret = execute(line, env)
        if (ret.__type === 'error') {
          return ret
        }
      }
      return ret
    }
    throw err('Unknown node', {node, nodes})
  }
  return execute(root.get('main').body, root)
}
function run(src) {
  const tokens = tokenize(src)
  const nodes = parse(tokens)
  const value = evaluate(nodes)
  return {tokens, nodes, value}
}

// tests
function testTokenize() {
  function t(expect, src) {
    test(expect, tokenize(src), {src})
  }
  t([
    { token: 'a', indent: 0, line: 1, index: 0, id: 'a' },
    { token: '=', indent: 0, line: 1, index: 2, sym: '=' },
    { token: '1', indent: 0, line: 1, index: 4, value: 1 }
  ], 'a = 1')
  t([
    { token: 'a', indent: 0, line: 1, index: 0, id: 'a' },
    { token: '=', indent: 0, line: 1, index: 2, sym: '=' },
    { token: '"a"', indent: 0, line: 1, index: 4, value: 'a' }
  ], 'a = "a"')
  t([
    { token: 'struct', indent: 0, line: 1, index: 0, id: 'struct' },
    { token: 'a', indent: 0, line: 1, index: 7, id: 'a' },
    { token: ':', indent: 0, line: 1, index: 8, sym: ':' },
    { token: 'value', indent: 2, line: 2, index: 12, id: 'value' },
    { token: 'a', indent: 2, line: 2, index: 18, id: 'a' },
    { token: 'method', indent: 2, line: 3, index: 22, id: 'method' },
    { token: 'offset', indent: 2, line: 3, index: 29, id: 'offset' },
    { token: ':', indent: 2, line: 3, index: 35, sym: ':' },
    { token: 'value', indent: 2, line: 3, index: 37, id: 'value' },
    { token: '+', indent: 2, line: 3, index: 43, op2: '+' },
    { token: 'offset', indent: 2, line: 3, index: 45, id: 'offset' }
  ], 'struct a:\n  value a\n  method offset: value + offset')
  t([
    { token: 'a', indent: 0, line: 1, index: 0, id: 'a' },
    { token: '=', indent: 0, line: 1, index: 2, sym: '=' },
    { token: '[]int', indent: 0, line: 1, index: 4, id: '[]int' }
  ], 'a = []int')
  t([], '# comment')
}
function testParse() {
  function show(node) {
    return node.value ? node.value.toString() :
      node.id ? node.id :
      node.body ? node.name + node.args.map(x => ' ' + x).join('') + ' = ' + show(node.body) :
      node.call ? node.call + '(' + node.args.map(show).join(' ') + ')' :
      node.op2 ? '(' + show(node.lhs) + ' ' + node.op2 + ' ' + show(node.rhs) + ')' :
      node.struct ? [node.name, ...node.args].join(' ') + ': ' + node.struct.join(' ') :
      node.adt ? [node.name, ...node.args].join(' ') + '| ' + node.adt.map(e => e.join(' ')).join(', ') :
      node.flow ? node.flow.map(show).join(' ; ') :
      (() => { throw err('Unknown', {node}) })()
  }
  function t(expect, src) {
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
}
function testEvaluate() {
  function t(expect, exp, ...defs) {
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
  t({__type: 'some', content: 1}, 'some(1)')
  t({__type: 'error', message: 'hi'}, 'error("hi")')
  t({__type: 'some', content: 3}, 'some(1).map(x => x + 2)')
  t({__type: 'error', message: 'hi'}, 'error("hi").map(x => x + 2)')
  t({__type: 'some', content: 1}, 'some(1).alt(2)')
  t({__type: 'some', content: 2}, 'error("hi").alt(2)')

  // flow
  t(2, '\n  a <- 1\n  a += 1\n  a')
  t({__type: 'error', message: 'hi'}, '\n  a <- error("hi")\n  1')

  // string methods
  t(5, '"hello".length')

  // array methods
  t(0, '[].size')
  t([1, 4], '[0 1+2].map(v => v + 1)')
}
function testMoa(base) {
  const baseNodes = parse(tokenize(base))

  function t(expect, src) {
    const nodes = baseNodes.concat(parse(tokenize('main = compile_js(' + JSON.stringify(src) + ')')))
    test(expect, evaluate(nodes).value, src)
  }
}

// main
function main() {
  testTokenize()
  testParse()
  testEvaluate()
  //testMoa(require('fs').readFileSync('moa.moa', {encoding: 'utf-8'}))
  puts('ok')
}
main()
