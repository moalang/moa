'use strict'
// helpers
function write(...a) { a.map(o => process.stdout.write(o.toString())) }
function puts(...a) { console.log(...a) }
function eq(a, b) { return a === b || str(a) === str(b) }
function str(o) { return JSON.stringify(o) }
function err(msg, o) { puts(o); return new Error(msg) }
function test(expect, message, actual) {
  if (eq(expect, actual)) {
    write('.')
  } else {
    puts('Failed', message)
    puts('expect:', expect)
    puts('actual:', actual)
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
    if (token[0].match(/^[a-zA-Z_]/)) { o.is_id = true }
    else if (token[0].match(/^[0-9.]+$/)) { o.is_num = true; o.value = parseInt(o.token) }
    else if (token[0].match(/^["'`]/)) { o.is_str = true; o.value = o.token.slice(1, -1) }
    else if (token[0].match(/^[=:|]$/)) { o.is_sym = true }
    else { o.is_op = true }
    return o
  }).filter(x => x.token.trim())
}
function parse(tokens) {
  const len = tokens.length
  let pos = 0
  function consume() {
    return tokens[pos++]
  }
  function until(f, g) {
    const a = []
    while (f(tokens[pos])) {
      a.push(g())
    }
    return a
  }
  function parse_define() {
    const [name, ...args] = until(x => x.is_id, consume)
    const sym = consume()
    switch (sym.token) {
      case '=': return {name, args, body: parse_body(), is_func: true}
      case ':': return {name, args, body: parse_struct(), is_struct: true}
      case '|': return {name, args, body: parse_enum(), is_enum: true}
      default: throw err('Unknown token', {sym,pos,tokens})
    }
  }
  function parse_body() {
    return consume()
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
  const env = {}
  for (const node of nodes) {
    env[node.name.token] = node
  }
  function run(node) {
    if (node.is_func) { return run(node.body) }
    if (node.value) { return node.value }
    throw err('Unknown node', {node})
  }
  return run(env.main)
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
    test(expect, src, tokenize(src))
  }
  t([
    { token: 'a', indent: 0, line: 1, index: 0, is_id: true },
    { token: '=', indent: 0, line: 1, index: 2, is_sym: true },
    { token: '1', indent: 0, line: 1, index: 4, is_num: true, value: 1 }
  ], 'a = 1')
  t([
    { token: 'a', indent: 0, line: 1, index: 0, is_id: true },
    { token: '=', indent: 0, line: 1, index: 2, is_sym: true },
    { token: '"a"', indent: 0, line: 1, index: 4, is_str: true, value: 'a' }
  ], 'a = "a"')
  t([
    { token: 'struct', indent: 0, line: 1, index: 0, is_id: true },
    { token: 'a', indent: 0, line: 1, index: 7, is_id: true },
    { token: ':', indent: 0, line: 1, index: 8, is_sym: true },
    { token: 'value', indent: 2, line: 2, index: 12, is_id: true },
    { token: 'a', indent: 2, line: 2, index: 18, is_id: true },
    { token: 'method', indent: 2, line: 3, index: 22, is_id: true },
    { token: 'offset', indent: 2, line: 3, index: 29, is_id: true },
    { token: '=', indent: 2, line: 3, index: 36, is_sym: true },
    { token: 'value', indent: 2, line: 3, index: 38, is_id: true },
    { token: '+', indent: 2, line: 3, index: 44, is_op: true },
    { token: 'offset', indent: 2, line: 3, index: 46, is_id: true }
  ], 'struct a:\n  value a\n  method offset = value + offset')
}
function testParse() {
  function t(expect, src) {
    const tokens = tokenize(src)
    const nodes = parse(tokens)
  }
  t('f = 1', 'f = 1')
}
function testEvaluate() {
  function t(expect, exp, ...defs) {
    defs.push('main = ' + exp)
    const src = defs.join('\n')
    test(expect, src, run(src).value)
  }
  t(1, '1')
  t('hi', '"hi"')
}

// main
function main() {
  testTokenize()
  testParse()
  testEvaluate()
  puts('ok')
}
main()
