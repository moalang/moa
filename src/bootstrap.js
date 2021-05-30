'use strict'
// helpers
function write(...a) { a.map(o => process.stdout.write(o.toString())) }
function puts(...a) { console.log(...a) }
function dump(o) { console.dir(o, {depth: null}) }
function eq(a, b) { return a === b || str(a) === str(b) }
function str(o) { return JSON.stringify(o) }
function err(msg, o) { puts(o); return new Error(msg) }
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
  return [...src.matchAll(/\s+|[a-zA-Z0-9_]+|[+\-*\/=><]+|".+?"|./g)].map(s => {
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
    else if (token[0].match(/^[=:|]$/)) { o.sym = o.token }
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
    const lhs = consume()
    let ret = lhs
    while (true) {
      const next = consume()
      if (!next) { return ret }
      if (next.op2) {
        const rhs = unit()
        if (rhs.op2) {
          const o1 = next.token
          const o2 = rhs.op2
          if ('*/'.includes(o1) && '+-'.includes(o2)) {
            return { op2: o2, lhs: { op2: o1, lhs, rhs: rhs.lhs }, rhs: rhs.rhs }
          } else {
            return { op2: o1, lhs, rhs }
          }
        }
        return {op2: next.token, lhs, rhs}
      } else {
        pos--
        return ret
      }
    }
  }
  function until(f, g) {
    const a = []
    while (f(tokens[pos])) {
      a.push(g())
    }
    return a
  }
  function parse_define() {
    const [name, ...args] = until(x => x.id, consume).map(t => t.token)
    const sym = consume()
    switch (sym.token) {
      case '=': return {name, args, body: parse_body()}
      case ':': return {name, args, struct: parse_struct()}
      case '|': return {name, args, enum: parse_enum()}
      default: throw err('Unknown token', {sym,pos,tokens})
    }
  }
  function parse_body() {
    return unit()
  }
  function parse_struct() {
  }
  function parse_enum() {
  }

  const nodes = []
  while (pos < len) {
    nodes.push(parse_define())
  }
  return nodes
}
function evaluate(nodes) {
  const environment = function(parent) {
    this.get = key => this['__' + key] || parent + ['__' + key] || (() => { throw err('not found', {key,dit,parent}) })()
    this.put = (key,value) => this['__' + key] = value
  }
  const root = new environment({})
  for (const node of nodes) {
    root.put(node.name, node)
  }
  function run(node, env) {
    if (node.value) { return node.value }
    if (node.body) { return run(node.body, env) }
    if (node.id) { return run(env.get(node.id), env) }
    if (node.op2) {
      switch (node.op2) {
        case '+': return run(node.lhs, env) + run(node.rhs, env)
        case '-': return run(node.lhs, env) - run(node.rhs, env)
        case '*': return run(node.lhs, env) * run(node.rhs, env)
        case '/': return run(node.lhs, env) / run(node.rhs, env)
        default: throw err('Unknown operator', node)
      }
    }
    throw err('Unknown node', {node, env})
  }
  return run(root.get('main').body, root)
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
      node.call ? node.name + '(' + node.args.map(show).join(' ') + ')' :
      node.op2 ? '(' + show(node.lhs) + ' ' + node.op2 + ' ' + show(node.rhs) + ')' :
      (() => { throw err('Unknown', {node}) })()
  }
  function t(expect, src) {
    const tokens = tokenize(src)
    const nodes = parse(tokens)
    test(expect, nodes.map(show).join('\n'), {src,nodes})
  }
  t('f = 1', 'f = 1')
  t('f a = (a + 1)', 'f a = a + 1')
  t('f a = (a + (1 * 2))', 'f a = a + 1 * 2')
  t('f a = ((a * 1) + 2)', 'f a = a * 1 + 2')
}
function testEvaluate() {
  function t(expect, exp, ...defs) {
    defs.push('main = ' + exp)
    const src = defs.join('\n')
    test(expect, run(src).value, src)
  }
  t(1, '1')
  t('hi', '"hi"')
  t(7, '1+2*3')
  t(5, '1*2+3')
  t(1, 'f', 'f=1')
  //t(1, 'f(1)', 'f a = a + 1')
}

// main
function main() {
  testTokenize()
  testParse()
  testEvaluate()
  puts('ok')
}
main()
