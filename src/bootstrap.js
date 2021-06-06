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

// main process
function tokenize(src) {
  let indent = 0
  let line = 1
  return [...src.matchAll(/\s+|[a-zA-Z0-9_]+|[+\-*\/=><]+|".+?"|[\[\]()]|./g)].map(s => {
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
    if (token[0].match(/^[a-zA-Z_]/)) { o.id = o.token }
    else if (token[0].match(/^[0-9.]+$/)) { o.value = parseInt(o.token) }
    else if (token[0].match(/^["'`]/)) { o.value = o.token.slice(1, -1) }
    else if (token[0].match(/^[=:|()\[\]]$/)) { o.sym = o.token }
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
  function unit() {
    return operator(consume())
  }
  function expect(token) {
    return pos < len && tokens[pos].token === token
  }
  function drop(token, value) {
    const node = consume()
    if (node.token !== token) { throw err('Unexpected token', {node, expect}) }
    return value
  }
  function operator(lhs) {
    if (lhs.sym === '[') {
      return operator({ list: drop(']', until(t => t.sym !== ']', unit)) })
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
  function parse_define() {
    const [name, ...args] = until(t => t.id, consume).map(t => t.token)
    const sym = consume()
    switch (sym.token) {
      case '=': return {name, args, body: parse_body()}
      case ':': return {name, args, struct: parse_struct(2)}
      case '|': return {name, args, adt: parse_adt()}
      default: throw err('Unknown token', {sym,pos,tokens})
    }
  }
  function parse_body() {
    return unit()
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
    this.get = key => d[key] || parent.get(key) || (() => { throw err('not found', {key,d,parent}) })()
    this.put = (key,value) => d[key] = value
    this.new = (keys,values) => {
      const env = new environment(this)
      keys.map((kv, i) => env.put(kv, values[i]))
      return env
    }
  }
  const root = new environment({get:()=>null})
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
  function run(node, env) {
    if (node.value) { return node }
    if (node.list) { node.value = node.list.map(x => run(x, env).value); return node }
    if (node.body) { return run(node.body, env) }
    if (node.id) { return run(env.get(node.id), env) }
    if (node.call) {
      const def = env.get(node.call)
      const argv = node.args.map(arg => run(arg, env))
      if (def.body) {
        return run(def.body, env.new(def.args, argv))
      } else if (def.struct) {
        return {value: newType(def.name, def.struct, argv.map(a => a.value))}
      } else if (def.adt) {
        return {value: newType(def.name, def.adt, argv.map(a => a.value))}
      } else {
        throw err('Unknown apply', def)
      }
    }
    if (node.op2) {
      const l = run(node.lhs, env).value
      const r = run(node.rhs, env).value
      switch (node.op2) {
        case '+': node.value = l + r; return node
        case '-': node.value = l - r; return node
        case '*': node.value = l * r; return node
        case '/': node.value = l / r; return node
        default: throw err('Unknown operator', node)
      }
    }
    throw err('Unknown node', {node, nodes})
  }
  return run(root.get('main').body, root).value
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
    { token: '=', indent: 2, line: 3, index: 36, sym: '=' },
    { token: 'value', indent: 2, line: 3, index: 38, id: 'value' },
    { token: '+', indent: 2, line: 3, index: 44, op2: '+' },
    { token: 'offset', indent: 2, line: 3, index: 46, id: 'offset' }
  ], 'struct a:\n  value a\n  method offset = value + offset')
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
  t([], '[]')

  // expression
  t(7, '1+2*3')
  t(5, '1*2+3')

  // function
  t(1, 'f', 'f=1')
  t(2, 'f(1)', 'f a = a + 1')
  t(3, 'f(1) + g(1)', 'f a = a', 'g a = a + f(a)')
  t(3, 'add(1 2)', 'add a b = a + b')

  // struct
  t({__type: 'wrap', value: 1}, 'wrap(1)', 'wrap:\n  value int')
  t({__type: 'vector2', x: 1, y: 2}, 'vector2(1 2)', 'vector2 a:\n  x a\n  y a')

  // adt
  t({__type: 'just', value: 1}, 'just(1)', 'maybe a|\n  just:\n    value a\n  none')
  t({__type: 'none'}, 'none', 'maybe a|\n  just:\n    value a\n  none')

  // control flow
  // pattern match for adt
  // effect

  // embedded function
  // string method
  // number method
}

// main
function main() {
  testTokenize()
  testParse()
  testEvaluate()
  puts('ok')
}
main()
