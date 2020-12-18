// utils
function str(obj) {
  return JSON.stringify(obj, null, 2)
}
function puts(...args) {
  for (const arg of args) {
    console.dir(arg, {depth: null})
  }
}

// compiler
function tokenize(src) {
  const consume = (pos,tag,m) => m ? ({tag, pos, code: typeof(m) === 'string' ? m : m[0]}) : null
  const match = (p,tag,r) => consume(p, tag, src.slice(p).match(r))
  const some = (p,tag,s) => consume(p, tag, s.split(' ').find(w => src.slice(p).startsWith(w)))
  const eat = p =>
    match(p, 'num', /^[0-9]+(\.[0-9]+)?/) ||
    match(p, 'id', /^[A-Za-z_][A-Za-z0-9_]*(,[A-Za-z_][A-Za-z0-9_]*)*/) ||
    match(p, 'str', /^"(?:(?:\\")|[^"])*"/) ||
    match(p, 'prop', /^.[A-Za-z_][A-Za-z0-9_]*/) ||
    match(p, 'spaces', /^[ #\n]+/) ||
    some(p, 'la', '[') ||
    some(p, 'ra', ']') ||
    some(p, 'lp', '(') ||
    some(p, 'rp', ')') ||
    some(p, 'op2', '+= -= *= /= || && == != >= <= ++ => := = : <- > < + - * /')

  let pos=0, tokens=[]
  while (pos < src.length) {
    const token = eat(pos)
    if (!token) { throw new Error('tokenize at ' + pos) }
    pos += token.code.length
    tokens.push(token)
  }

  const dst = tokens.map(t => t.code).join('')
  if (src !== dst) throw new Error('tokenize assertion: src=' + str(src) + ' dst=' + str(dst))
  let indent = 0
  for (const token of tokens) {
    if (token.tag === 'spaces') {
      indent = token.code.split('\n').slice(-1)[0].length
      if (indent % 2 != 0) { throw new Error('invalid indent=' + indent + ' at ' + token.pos) }
    } else {
      token.indent = indent
    }
  }
  return tokens.filter(t => t.tag !== 'spaces')
}
function parse(tokens) {
  const nodes = []
  let pos = 0
  function info() {
    return ' at=' + pos + ' tokens=' + str(tokens) + ' nodes=' + nodes
  }
  function look() {
    return (tokens[pos+1] || {tag:'EOT'}).tag
  }
  function until(end) {
    const ary = []
    let t
    while ((t = eat()).code !== end) {
      ary.push(t)
    }
    return ary
  }
  function setArgv(token) {
    token.argv = []
    const next = look()
    if (next.code === '(' && next.pos === token.pos + token.length) {
      eat()
      token.argv = until(')')
    }
  }
  function eat() {
    const token = tokens[pos++]
    switch (look()) {
      case 'EOT':
      case 'num':
      case 'str':
      case 'ra':
      case 'rp':
        return token

      case 'id':
        setArgv(token)
        return token

      case 'op2':
        const op2 = eat()
        if (op2.code !== 'op2') { throw new Error('Bug '+ str(op2)) }
        op2.lhs =token
        op2.rhs = eat()
        return op2

      case 'prop':
        const prop = eat()
        prop.target = nodes[node.length - 1]
        setArgv(prop)
        return prop

      case 'la':
        token.ary = until(']')
        return token

      case 'lp':
        token.exp = eat()
        if (eat().code !== ')') { throw new Error('Invalid end of parentheses' + info()) }
        return token

        if (token.code === '(') {
        } else if (token.code === ']') {
          token.ary = until(']')
          return token
        } else {
          throw new Error('Bug ' + str(token))
        }

      default:
          throw new Error('Bug ' + str(look()))
    }
  }
  while (pos < tokens.length) {
    const node = eat()
    if (!node) { throw new Error('failed to parse at=' + pos + ' tokens=' + str(tokens)) }
    nodes.push(node)
  }
  return nodes
}
function compile(token) {
  puts(token)
  return token
}
function main() {
  const src = require('fs').readFileSync('/dev/stdin', 'utf8')
  const tokens = tokenize(src)
  const defs = parse(tokens)
  const js = defs.map(compile).join('\n')
  console.log(js)
}
main()
