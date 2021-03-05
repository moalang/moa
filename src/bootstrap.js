'use strict'
const print = (...args) => console.log(...args)
const dump = s => JSON.stringify(s)
const op2 = '&& || == != <- >= <= > < += -= *= /= %= ++ + - * / % .'.split(' ')
const syms = ': | = [ ] ( )'.split(' ')
const range = (s,e,step) => {
  const a = []
  for (let i=s; i<e; i+=step) {
    a.push(i)
  }
  return a
}
const seqBy = (a,f) => {
  if (!a.length) { return [] }
  const l = a.length
  const ret = []
  let sep = [a[0]]
  let prev = f(a[0])
  for (let i = 1; i<l; ++i) {
    const v = a[i]
    const key = f(v)
    if (key === prev) {
      sep.push(v)
    } else {
      ret.push(sep)
      sep = [v]
      prev = key
    }
  }
  if (sep.length) { ret.push(sep) }
  return ret
}
const runtime = (function() {
  function __equals(a, b) {
    const t1 = typeof(a)
    const t2 = typeof(b)
    if (t1 === 'object' && t2 === 'function' && a.__tag) { return a.__tag === b.name }
    if (t1 !== t2) { return false }
    if (t1 === 'object') { return JSON.stringify(a) === JSON.stringify(b) }
    return a == b
  }
}).toString().split('\n').slice(1,-1).join('\n')
function tokenize(src) {
  const len = src.length
  let pos = 0
  let indent = 0
  let lineno = 1
  const newToken = (tag, code) => ({tag, code, lineno, indent, pos})
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
    if (token.code.includes('\n')) {
      const lines = token.code.split('\n')
      indent = lines[lines.length - 1].length
      lineno += lines.length - 1
    }
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
  function until(f, g) {
    g = g || parse_exp
    const exps = []
    while (pos < len && f(tokens[pos])) {
      exps.push(g())
    }
    return exps
  }
  function parse_define() {
    const fname = consume(t => t.tag === 'id')
    const args = until(t => t.tag === 'id')
    const sym = consume(t => t.code === '=' || t.code === ':' || t.code === '|')
    sym.fname = fname
    sym.args = args
    if (sym.code === '=') {
      sym.exps = parse_body(sym)
    } else if (sym.code === ':') {
      const fields = until(t => t.indent > sym.indent)
      if (fields.length %2 !== 0) { throw new Error('Definition of struct should have even the number of fields: ' + dump(fields)) }
      sym.struct = fields
    } else if (sym.code === '|') {
      const fields = until(t => t.indent > sym.indent)
      const tags = seqBy(fields, t => t.lineno)
      if (fields.length %2 !== 0) { throw new Error('Definition of struct should have even the number of fields: ' + dump(fields)) }
      sym.adt = tags
    } else {
      throw new Error('Unexpected symbol: ' + dump(sym))
    }
    return sym
  }
  function is_define() {
    let p = pos
    while (p < len && tokens[p].tag === 'id') {
      p++
    }
    return p > pos && p < len && tokens[p].code === '='
  }
  function parse_body(base) {
    if (look().lineno === base.lineno) {
      return [parse_exp()]
    } else if (look().lineno > base.lineno) {
      return until(t => t.indent > base.indent, () => is_define() ? parse_define() : parse_exp())
    } else {
      throw new Error('Unexpected function body: ' + dump(base))
    }
  }
  function parse_exp() {
    let l = parse_unit()
    while (op2.includes(look().code)) {
      let op = parse_unit()
      op.lhs = l
      op.rhs = parse_exp()
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
          token.argv = until(t => t.code !== ')')
          consume(t => t.code === ')')
        }
        return token
      case 'num':
      case 'str':
      case 'op2': return token
      case 'sym':
          switch (token.code) {
            case '[':
              token.list = until(u => u.code !== ']')
              consume(t => t.code === ']')
              return token
            case '(':
              token.body = parse_exp()
              consume(t => t.code === ')')
              return token
            default: return token
          }
      default: throw new Error('Unexpected token: ' + dump(token))
    }
  }
  const defines = []
  while (pos < len) {
    defines.push(parse_define())
  }
  return defines
}
function generate(nodes) {
  function genId(id, argv) {
    if (id === 'if') {
      const a = argv.map(gen)
      const cases = range(1, a.length, 2).map(i => `${a[i-1]} ? ${a[i]} : `).join('')
      return '(' + cases + a[a.length-1] + ')'
    } else if (id === 'match') {
      const a = argv.map(gen)
      const cond = i => a[i-1] === '_' ? 'true' : `__equals(__m, ${a[i-1]})`
      const cases = range(2, a.length, 2).map(i => `${cond(i)} ? ${a[i]} : `).join('')
      const otherwise = '(() => { throw new Error("Failed to match")})()'
      const js = cases + otherwise
      return `(__m => ${js})(${a[0]})`
    } else {
      return id + (argv ? '(' + argv.map(gen).join(', ') + ')' : '')
    }
  }
  function genFunc(fname, args, exps) {
    if (exps.length === 0) {
      throw new Error('Empty exps: ' + fname)
    }

    const lines = exps.map(gen)
    const fbody = () => lines.slice(0, -1).join('\n') + '\nreturn ' + lines[lines.length - 1]
    const exp = lines.length === 1 ? lines[0] : `(() => {${ fbody() }})()`
    const body = args.length > 0 ? `(${args.map(t => t.code).join(',')}) => ${exp}` : exp
    return `const ${fname} = ${body}`
  }
  function genStruct(fname, _args, struct) {
    const fields = range(1, struct.length, 2).map(i => struct[i - 1].code).join(',')
    return `const ${fname} = (${fields}) => ({${fields}})`
  }
  function genAdt(fname, _args, adt) {
    const g = (tag, fields) => `const ${tag} = (${fields}) => ({${fields}, __tag: '${tag}'})`
    const f = a => g(a[0].code, range(1, a.length, 2).map(i => a[i].code).join(', '))
    return adt.map(f).join('\n')
  }
  function genAssign(lhs, rhs) {
    if (rhs.code === 'var') {
      return `let ${gen(lhs)} = ${gen(rhs.argv[0])}`
    } else {
      return `${gen(lhs)} = ${gen(rhs)}`
    }
  }
  function gen(node) {
    if (!node) { throw new Error('node is not defined: ' + dump(node)) }
    switch (node.tag) {
      case 'id': return genId(node.code, node.argv)
      case 'num':
      case 'str': return node.code
      case 'op2':
        switch (node.code) {
          case '++': return `${gen(node.lhs)}.concat(${gen(node.rhs)})`
          case '<-': return genAssign(node.lhs, node.rhs)
          default: return `${gen(node.lhs)} ${node.code} ${gen(node.rhs)}`
        }
      case 'sym':
        switch (node.code) {
          case '=': return genFunc(node.fname.code, node.args, node.exps)
          case ':': return genStruct(node.fname.code, node.args, node.struct)
          case '|': return genAdt(node.fname.code, node.args, node.adt)
          case '[': return '[' + node.list.map(gen).join(', ') + ']'
          case '(': return '(' + gen(node.body) + ')'
          default: throw Error('Gen sym error ' + dump(node))
        }
      default: throw Error('Gen error ' + dump(node))
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
    actual = Function(runtime + '\n' + js + '\nreturn typeof main === "function" ? main() : main')()
  } catch (e) {
    error = e
  }
  return {tokens, nodes, js, actual, error}
}
function testAll() {
  const eq = (expect, exp, ...funcs) => {
    funcs.reverse()
    funcs.push('main = ' + exp)
    const src = funcs.join('\n')
    const result = run(src)
    const actual = result.actual
    if (dump(expect) === dump(actual)) {
      process.stdout.write('.')
    } else {
      print('Failed')
      print('js    : ', result.js)
      print('src   : ', src)
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
  eq(6, 'add(1 add(2 3))', 'add a b = a + b')

  // control flow
  eq(1, 'if(true 1 lazy)')
  eq(2, 'if(false lazy 2)')
  eq(3, 'if(false lazy true 3 lazy)')
  eq(3, 'if(false lazy false lazy 3)')
  eq(2, 'match(1 1 2 lazy lazy)')
  eq(4, 'match(3 1 2 3 4)')
  eq(2, 'match(3 _ 2 3 4)')

  // struct
  eq({a: 'hi', b: 1}, 'struct("hi" 1)', 'struct:\n   a string\n  b int')
  eq('hi', 'struct("hi" 1).a', 'struct:\n   a string\n  b int')
  eq(1, 'struct("hi" 1).b', 'struct:\n   a string\n  b int')

  // adt
  eq(1, 'match(v aint v.vint abool v.vbool)', 'v = aint(1)', 'adt|\n  aint vint int\n  abool vbool bool')
  eq(true, 'match(v aint v.vint abool v.vbool)', 'v = abool(true)', 'adt|\n  aint vint int\n  abool vbool bool')

  // effect
  eq(3, '\n  a = 1\n  a + 2')
  eq(1, '\n  a <- f\n  a', 'f = 1')
  eq(4, '\n  a <- var(1)\n  a += 1\n  a*=2\n  a')
  eq(1, '\n  a <- var(6)\n  a /= 2\n  a%=2\n  a')
  print('ok')
}
testAll()
