'use strict'
const print = (...args) => console.log(...args)
const dump = s => JSON.stringify(s)
const op2 = '&& || == != >= > <= < ++ + - * /'.split(' ')
const syms = '= [ ] ( )'.split(' ')
const range = (s,e,step) => {
  const a = []
  for (let i=s; i<e; i+=step) {
    a.push(i)
  }
  return a
}
function tokenize(src) {
  const len = src.length
  let pos = 0
  let indent = 0
  const newToken = (tag, code) => ({tag, code, pos, indent})
  const reg = (tag, m) => m && newToken(tag, m[0])
  const any = (tag, m) => m && newToken(tag, m)
  const rule = s =>
    reg('id', s.match(/^[A-Za-z_][A-Za-z_0-9]*/)) ||
    reg('num', s.match(/^[0-9]+/)) ||
    reg('str', s.match(/^"[^"]*"/)) ||
    reg('str', s.match(/^`[^`]*`/)) ||
    any('op2', op2.find(a => s.startsWith(a))) ||
    any('sym', syms.find(a => s.startsWith(a))) ||
    reg('spaces', s.match(/^[ \t\r\n]+/))
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
  function consume(f) {
    if (pos >= len) { throw new Error('Out of index: ' + dump({len,pos,tokens})) }
    const token = tokens[pos]
    if (f && !f(token)) { throw new Error(`Unexpected ${f.toString()} ${dump(token)}`) }
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
    const fname = consume(t => t.tag === 'id')
    const args = consumes(t => t.tag === 'id')
    const sym = consume(t => t.code === '=')
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
    const next = look()
    switch (token.tag) {
      case 'id':
        if (next.code === '(' && next.pos === token.pos + token.code.length) {
          consume(t => t.code === '(')
          token.argv = until(t => t.code !== ')').slice(0, -1)
        }
        return token
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
              consume(t => t.code === ')')
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
    if (args.length > 0) {
      return `const ${fname} = (${args.map(t => t.code).join(',')}) => ` + gen(body)
    } else {
      return `const ${fname} = ${gen(body)}`
    }
  }
  function genId(id, argv) {
    if (id === 'if') {
      const a = argv.map(gen)
      return '(' + range(1, a.length, 2).map(i => `${a[i-1]} ? ${a[i]} : `).join('') + a[a.length-1] + ')'
    } else {
      return id + (argv ? '(' + argv.map(gen).join(', ') + ')' : '')
    }
  }
  function gen(token) {
    switch (token.tag) {
      case 'id': return genId(token.code, token.argv)
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
  const eq = (expect, exp, ...funcs) => {
    funcs.push('main = ' + exp)
    const src = funcs.join('\n')
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
  eq(1, 'a', 'a=1')
  eq(3, 'add(1 2)', 'add a b = a + b')

  // control flow
  eq(1, 'if(true 1 lazy)')
  eq(2, 'if(false lazy 2)')
  eq(3, 'if(false lazy true 3 lazy)')
  eq(3, 'if(false lazy false lazy 3)')

  // struct

  // enum

  // effect
  print('ok')
}
testAll()
