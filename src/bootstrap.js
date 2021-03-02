'use strict'
const print = (...args) => console.log(...args)
const dump = s => JSON.stringify(s)
const op2 = '&& || == != >= > <= < ++ + - * /'.split(' ')
const syms = '= [ ] ( )'.split(' ')
function tokenize(src) {
  const len = src.length
  let pos = 0
  let indent = 0
  const newToken = (tag, code) => ({tag, code, pos, indent})
  const find = (tag, m) => m ? newToken(tag, typeof m === 'string' ? m : m[0]) : null
  const rule = s =>
    find('id', s.match(/^[A-Za-z_][A-Za-z_0-9]*/)) ||
    find('num', s.match(/^[0-9]+/)) ||
    find('str', s.match(/^"[^"]*"/)) ||
    find('str', s.match(/^`[^`]*`/)) ||
    find('op2', op2.find(a => s.startsWith(a))) ||
    find('sym', syms.find(a => s.startsWith(a))) ||
    find('spaces', s.match(/^[ \t\r\n]+/))
  const tokens = []
  while (pos < len) {
    const token = rule(src.slice(pos))
    if (!token) { throw new Error('Failed to tokenize: ' + src.slice(pos)) }
    pos += token.code.length
    tokens.push(token)
  }
  return tokens.filter(x => x.tag !== 'spaces')
}
function parse(tokens) {
  const len = tokens.length
  let pos = 0

  const look = () => tokens[pos] || {}
  function consume(tag) {
    if (pos >= len) { throw new Error('Out of index: ' + dump({len,pos,tokens})) }
    const token = tokens[pos]
    if (tag && token.tag !== tag) { throw new Error(`Unexpected ${tag}: ${dump(token)}`) }
    pos++
    return token
  }
  function consumes(f) {
    const tokens_ = []
    while (pos < len && f(tokens[pos])) {
      tokens_.push(parse_unit())
    }
    return tokens_
  }
  function until(f) {
    const units = []
    while (pos < len) {
      const unit = parse_unit()
      units.push(unit)
      if (!f(unit)) { break }
    }
    return units
  }
  function parse_function() {
    const fname = consume('id')
    const args = consumes(t => t.tag === 'id')
    const sym = consume('sym')
    if (sym.code !== '=') { throw new Error('define function should contains =: ' + dump(eq)) }
    sym.fname = fname
    sym.args = args
    sym.body = parse_body()
    return sym
  }
  function parse_body() {
    let l = parse_unit()
    while (op2.includes(look().code)) {
      let op = parse_unit()
      op.lhs = l
      op.rhs = parse_unit()
      l = op
    }
    return l
  }
  function parse_unit() {
    const token = consume()
    switch (token.tag) {
      case 'id':
      case 'num':
      case 'str':
      case 'op2': return token
      case 'sym':
          switch (token.code) {
            case '[':
              token.list = until(u => u.code !== ']').slice(0, -1)
              return token
            case '(':
              token.body = parse_body()
              if (consume('sym').code !== ')') { throw new Error('Not found close parenthese') }
              return token
            default: return token
          }
      default: throw new Error('Unexpected token: ' + dump(token))
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
      case 'id':
      case 'num':
      case 'str': return token.code
      case 'op2':
        switch (token.code) {
          case '++': return `${gen(token.lhs)}.concat(${gen(token.rhs)})`
          default: return `${gen(token.lhs)} ${token.code} ${gen(token.rhs)}`
        }
      case 'sym':
        switch (token.code) {
          case '=': return genFunc(token.fname.code, token.args, token.body)
          case '[': return '[' + token.list.map(gen).join(', ') + ']'
          case '(': return '(' + gen(token.body) + ')'
          default: throw Error('Gen sym error ' + dump(token))
        }
      default: throw Error('Gen error ' + dump(token))
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
    if (dump(expect) === dump(actual)) {
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
  eq('hi', '"hi"')
  eq('"hi"', '`"hi"`')
  eq([], '[]')
  eq([1], '[1]')
  eq([1, 2], '[1 2]')
  eq(['a', 'b'], '["a" "b"]')
  eq(true, 'true')
  eq(false, 'false')

  // exp
  eq(3, '1 + 2')
  eq(7, '1 + 2 * 3')
  eq(9, '(1 + 2) * 3')
  eq(2, '4 / 2')
  eq('ab', '"a" ++ "b"')
  eq([1, 2], '[1] ++ [2]')
  eq(false, 'true && false')
  eq(true, 'false || true')
  eq(false, '1 == 2')
  eq(true, '1 != 2')
  eq(true, '2 >= 2')
  eq(false, '2 > 2')
  eq(true, '2 <= 2')
  eq(false, '2 < 2')
  eq(true, '1 == 1 && 1 == 1')
  eq(false, '1 == 1 && 1 == 2')

  // function

  // struct

  // enum

  // effect

  // control flow
  print('ok')
}
testAll()
