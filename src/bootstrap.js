'use strict'
const print = (...args) => console.log(...args)
const str = s => JSON.stringify(s)
function tokenize(src) {
  const len = src.length
  let pos = 0
  let indent = 0
  const newToken = (tag,code) => ({tag,code,pos,indent})
  const find = (tag, m) => m ? newToken(tag, m[0]) : null
  const rule = s =>
    find('id', s.match(/^[A-Za-z_][A-Za-z_0-9]*/)) ||
    find('num', s.match(/^[0-9]+/)) ||
    find('sym', s.match(/^[+\-*/=]+/)) ||
    find('spaces', s.match(/^[ \t\r\n]+/))
  const tokens = []
  while (pos < len) {
    const token = rule(src.slice(pos))
    pos += token.code.length
    tokens.push(token)
  }
  return tokens.filter(x => x.tag !== 'spaces')
}
function parse(tokens) {
  const len = tokens.length
  let pos = 0

  function consume(tag) {
    if (pos >= len) { throw new Error('Out of index: ' + pos) }
    const token = tokens[pos]
    pos++
    if (tag && token.tag !== tag) { throw new Error('Unexpected tag: ' + str(tag)) }
    return token
  }
  function until(f) {
    const units = []
    while (pos < len && f(tokens[pos])) {
      units.push(parse_unit())
    }
    return units
  }
  function parse_function() {
    const fname = consume('id')
    const args = until(t => t.tag === 'id')
    const sym = consume('sym')
    if (sym.code !== '=') { throw new Error('define function should contains =: ' + str(eq)) }
    sym.fname = fname
    sym.args = args
    sym.body = parse_unit()
    return sym
  }
  function parse_unit() {
    const token = consume()
    switch (token.tag) {
      case 'sym': return token
      case 'num': return token
      default: throw new Error('Unexpected token: ' + str(token))
    }
  }
  const defines = []
  while (pos < len) {
    defines.push(parse_function())
  }
  return defines
}
function generate(nodes) {
  function genFunc(fname, args, body) {
    return `const ${fname} = (${args.map(t => t.code).join(',')}) => ` + gen(body)
  }
  function gen(token) {
    switch (token.tag) {
      case 'sym':
        switch (token.code) {
          case '=': return genFunc(token.fname.code, token.args, token.body)
          default: throw Error('Gen sym error ' + str(token))
        }
      case 'num': return token.code
      default: throw Error('Gen error ' + str(token))
    }
  }
  return nodes.map(gen).join('\n')
}
function run(src) {
  const tokens = tokenize(src)
  const nodes = parse(tokens)
  const js = generate(nodes)
  let actual = null
  let error = null
  try {
    actual = Function("'use strict'\n" + js + '\nreturn typeof main === "function" ? main() : main')()
  } catch (e) {
    error = e
  }
  return {tokens, nodes, js, actual, error}
}
function testAll() {
  const eq = (expect, exp) => {
    const src = 'main = ' + exp
    const result = run(src)
    const actual = result.actual
    if (expect === actual) {
      process.stdout.write('.')
    } else {
      print('Failed: ', src)
      print('js    : ', result.js)
      print('expect: ', expect)
      print('actual: ', actual)
      print('tokens: ', result.tokens)
      print('nodes : ', result.nodes)
      print('error : ', result.error)
    }
  }

  // value
  eq(1, '1')

  // exp

  // function

  // struct

  // enum

  // effect

  // control flow
  print('ok')
}
testAll()
