// utils
function str(obj) {
  return JSON.stringify(obj, null, 2)
}
function put(s, ...args) {
  process.stdout.write(s)
  for (const arg of args) {
    console.log('', arg)
  }
}
function puts(...args) {
  console.dir(args, {depth: null})
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
    match(p, 'prop', /^\.[A-Za-z_][A-Za-z0-9_]*/) ||
    match(p, 'spaces', /^[ #\n]+/) ||
    some(p, 'la', '[') ||
    some(p, 'ra', ']') ||
    some(p, 'lp', '(') ||
    some(p, 'rp', ')') ||
    some(p, 'op2', '+= -= *= /= || && == != >= <= ++ => := = : <- > < + - * /')

  let indent = 0
  let pos=0, tokens=[]
  while (pos < src.length) {
    const token = eat(pos)
    if (!token) { throw new Error('tokenize at ' + pos) }
    if (token.tag === 'spaces' && token.code.includes('\n')) {
      indent = token.code.split('\n').slice(-1)[0].length
      if (indent % 2 != 0) { throw new Error('invalid indent=' + indent + ' at ' + token.pos) }
    }
    token.indent = indent
    pos += token.code.length
    tokens.push(token)
  }

  const dst = tokens.map(t => t.code).join('')
  if (src !== dst) throw new Error('tokenize assertion: src=' + str(src) + ' dst=' + str(dst))
  return tokens.filter(t => t.tag !== 'spaces')
}
function parse(tokens) {
  const nodes = []
  const eot = {tag: 'EOT', code: ''}
  let pos = 0
  function info() {
    return ' at=' + pos + ' tokens=' + str(tokens) + ' nodes=' + str(nodes)
  }
  function until(end) {
    const ary = []
    let t
    while (pos < tokens.length && (t = parseTop()) && t.code !== end) {
      ary.push(t)
    }
    return ary
  }
  function parseCall() {
    if (pos >= tokens.length) { return [] }
    return tokens[pos].tag === 'lp' ? until(')') : []
  }
  function parseTop() {
    return parseLeft(parseUnit())
  }
  function parseUnit() {
    const token = tokens[pos++]
    switch (token.tag) {
      case 'num':
      case 'str':
      case 'ra':
      case 'rp': return token
      case 'id': token.argv = parseCall(); return token
      case 'la': token.ary = until(']'); return token
      case 'lp': token.items = until(')'); return token
      default:
        throw new Error('Unexpected tag ' + str(token) + info())
    }
  }
  function parseLeft(token) {
    if (pos >= tokens.length) { return token }
    if (token.tag === 'rp' || token.tag === 'ra') { return token }
    const next = tokens[pos]
    if (next.tag === 'op2') {
      ++pos
      next.lhs = token
      next.rhs = parseTop()
      return parseLeft(next)
    } else if (next.tag === 'prop') {
      ++pos
      next.target = token
      next.argv = parseCall()
      return parseLeft(next)
    } else {
      return token
    }
  }
  let next
  while (pos < tokens.length) {
    let node = parseTop()
    if (!node) { throw new Error('failed to parse at=' + pos + ' tokens=' + str(tokens)) }
    nodes.push(node)
  }

  for (const node of nodes) {
    if (node.tag === 'op2' && (!node.lhs || !node.rhs)) { throw new Error('Invalid ' + str(node)) }
    if (node.tag === 'prop' && (!node.target || !node.argv)) { throw new Error('Invalid ' + str(node)) }
    if (node.tag === 'la' && (!node.ary)) { throw new Error('Invalid ' + str(node)) }
    if (node.tag === 'lp' && (!node.items)) { throw new Error('Invalid ' + str(node)) }
    if (node.tag === 'ra') { throw new Error('Invalid ' + str(node)) }
    if (node.tag === 'rp') { throw new Error('Invalid ' + str(node)) }
  }

  return nodes
}
function generate(defs) {
  function genCall(argv) {
    return argv.length === 0 ? '' : '(' + argv.map(gen).join(',') + ')'
  }
  function gen(token) {
    switch (token.tag) {
      case 'id':
      case 'num':
      case 'str': return token.code
      case 'la': return '[' + token.ary.map(gen).join(',') + ']'
      case 'lp': return '(' + token.items.map(gen).join('') + ')'
      case 'prop': return gen(token.target) + genCall(prop.argv)
      case 'op2':
        switch (token.code) {
          case '=': return 'const ' + gen(token.lhs) + token.code + gen(token.rhs)
          default: return gen(token.lhs) + token.code + gen(token.rhs)
        }
      default: throw new Error('gen ' + str(token))
    }
  }
  return defs.map(gen).join("\n")
}
function compile(src) {
  const tokens = tokenize(src)
  const defs = parse(tokens)
  const js = generate(defs)
  return {tokens,defs,js}
}
function evalInSandbox(js) {
  try {
    return Function(js)()
  } catch (e) {
    puts('Failed to evaluate')
    for (const [i, line] of js.split('\n').entries()) {
      put((1 + i).toString().padStart(3) + ':', line)
    }
    return e
  }
}
function testAll() {
  function eq(expect, main) {
    const src = 'main = ' + main
    const info = compile(src)
    const js = info.js + '\nreturn main.valueOf()'
    const actual = evalInSandbox(js)
    if (str(expect) === str(actual)) {
      put('.')
    } else {
      console.error('Failed')
      put('expect: ', expect)
      put('actual: ', actual)
      put('src   : ', src)
      put('js    : ', info.js)
      put('defs  : '); puts(...info.defs)
      process.exit(1)
    }
  }

  // basic values
  eq(1, '1')
  eq(true, 'true')
  eq('a', '"a"')
  eq([], '[]')
  eq([1, 2, 3], '[1 2 3]')

  // expression
  eq(3, '1+2')
  eq(9, '(1 + 2) * 3')

  // check spaces handling
  eq(1, ' 1 ')

  console.log('ok')
}
function compileStdin() {
  const src = require('fs').readFileSync('/dev/stdin', 'utf8')
  const tokens = tokenize(src)
  const defs = parse(tokens)
  const js = compile(defs).js
  console.log(js)
}
function main() {
  process.argv[2] === 'test' ? testAll() : compileStdin()
}
main()
