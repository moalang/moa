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
  let pos = 0
  function info() {
    return ' at=' + pos + ' tokens=' + str(tokens) + ' nodes=' + str(nodes)
  }
  function nextTag() {
    return (tokens[pos] || {tag: 'EOT'}).tag
  }
  function until(end) {
    const ary = []
    let t
    while ((t = eat()).code !== end) {
      ary.push(t)
    }
    return ary
  }
  function eatArgv(token) {
    return nextTag() === 'lp' ? until(')') : []
  }
  function eat() {
    const token = tokens[pos++]
    switch (nextTag()) {
      case 'op2':
        const op2 = eat()
        op2.lhs = token
        op2.rhs = eat()
        return op2

      case 'prop':
        const prop = eat()
        prop.target = nodes[node.length - 1]
        prop.argv = eatArgv()
        return prop

     default:
       switch (token.tag) {
         case 'num': return token
         case 'str': return token
         case 'ra': return token
         case 'rp': return token
         case 'op2': return token
         case 'id': token.argv = eatArgv(); return token
         case 'la': token.ary = until(']'); return token
         case 'lp': token.items = until(')'); return token
         default:
           throw new Error('Unexpected tag ' + str(token) + info())
       }
    }
  }
  while (pos < tokens.length) {
    const node = eat()
    if (!node) { throw new Error('failed to parse at=' + pos + ' tokens=' + str(tokens)) }
    nodes.push(node)
  }
  return nodes
}
function generate(token) {
  function genCall(argv) {
    return argv.length === 0 ? '' : '(' + argv.map(generate).join(',') + ')'
  }
  switch (token.tag) {
    case 'id':
    case 'num':
    case 'str': return token.code
    case 'la': return '[' + token.ary.map(generate).join(',') + ']'
    case 'lp': return '(' + token.items.map(generate).join(',') + ')'
    case 'op2': return generate(token.lhs) + token.code + generate(token.rhs)
    case 'prop': return generate(token.target) + genCall(prop.argv)
    default:
      throw new Error('Bug ' + str(token))
  }
}
function compile(src) {
  const tokens = tokenize(src)
  const defs = parse(tokens)
  const js = defs.map(generate).join('\n')
  return js
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
    const js = compile(src) + '\nreturn main.valueOf()'
    const actual = evalInSandbox(js)
    if (str(expect) === str(actual)) {
      put('.')
    } else {
      console.error('Failed')
      put('expect: ', expect)
      put('actual: ', actual)
      put('js: ', js)
      process.exit(1)
    }
  }

  // basic values
  eq(1, '1')
  eq(true, 'true')
  eq('a', '"a"')
  eq([], '[]')
  eq([1, 2, 3], '[1 2 3]')

  // check parenthese handling
  eq(1, '(1)')

  // check spaces handling
  eq(1, ' 1 ')

  puts('ok')
}
function compileStdin() {
  const src = require('fs').readFileSync('/dev/stdin', 'utf8')
  const tokens = tokenize(src)
  const defs = parse(tokens)
  const js = defs.map(compile).join('\n')
  console.log(js)
}
function main() {
  process.argv[2] === 'test' ? testAll() : compileStdin()
}
main()
