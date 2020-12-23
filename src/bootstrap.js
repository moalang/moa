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
function copy(obj) {
  return JSON.parse(JSON.stringify(obj))
}
function zip(a, b) {
  const c = []
  const len = a.length < b.length ? a.length : b.length
  for (let i=0; i<len; ++i) {
    c.push([a[i], b[i]])
  }
  return c
}
function dict(keys, vals) {
  const d = {}
  if (keys.length !== vals.length) { throw new Error('dict ' + str({keys,vals})) }
  for (let i=0; i<keys.length; ++i) {
    d[keys[i]] = vals[i]
  }
  return d
}

// compiler
function tokenize(src) {
  const consume = (pos,tag,m) => m ? ({tag, pos, code: typeof(m) === 'string' ? m : m[0]}) : null
  const match = (p,tag,r) => consume(p, tag, src.slice(p).match(r))
  const some = (p,tag,s) => consume(p, tag, s.split(' ').find(w => src.slice(p).startsWith(w)))
  const eat = p =>
    match(p, 'func', /^[A-Za-z_][A-Za-z0-9_]*( +[A-Za-z_][A-Za-z0-9_]*)* +=/) ||
    match(p, 'enum', /^[A-Za-z_][A-Za-z0-9_]* enum:(\n  .+)+/) ||
    match(p, 'struct', /^[A-Za-z_][A-Za-z0-9_]* struct:(\n  .+)+/) ||
    match(p, 'num', /^[0-9]+(\.[0-9]+)?/) ||
    match(p, 'id', /^[A-Za-z_][A-Za-z0-9_]*(,[A-Za-z_][A-Za-z0-9_]*)*\(?/) ||
    match(p, 'str', /^"(?:(?:\\")|[^"])*"/) ||
    match(p, 'prop', /^\.[A-Za-z_][A-Za-z0-9_]*\(?/) ||
    match(p, 'spaces', /^[ #\n]+/) ||
    some(p, 'la', '[') ||
    some(p, 'ra', ']') ||
    some(p, 'lp', '(') ||
    some(p, 'rp', ')') ||
    some(p, 'op2', '+= -= *= /= || && == != >= <= ++ => := : <- -> > < + - * /')

  let indent = 0
  let pos=0, tokens=[]
  while (pos < src.length) {
    const token = eat(pos)
    if (!token) { throw new Error('tokenize at ' + pos) }
    if (token.tag === 'spaces' && token.code.includes('\n')) {
      const last = token.code.split('\n').slice(-1)[0]
      if (!last.includes('#')) {
        indent = last.length
        if (indent % 2 != 0) { throw new Error('invalid indent=' + indent + ' at ' + token.pos) }
      }
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
  function until(f) {
    const ary = []
    let t
    while (pos < tokens.length && f(t = parseTop())) {
      ary.push(t)
    }
    return ary
  }
  function parseCall(token) {
    if (token.code.endsWith('(')) {
      token.code = token.code.slice(0, -1)
      token.argv = until(t => t.tag !== 'rp')
    } else {
      token.argv = []
    }
  }
  function parseTop() {
    return parseLeft(parseUnit())
  }
  function parseIndent(t1, t2) {
    if (t1.indent < t2.indent) {
      t2.lines = [copy(t2)].concat(until(t => t.indent >= t2.indent))
    }
    return t2
  }
  function parseUnit() {
    const token = tokens[pos++]
    switch (token.tag) {
      case 'num':
      case 'str':
      case 'ra':
      case 'rp': return token
      case 'func':
        const ids = token.code.replace('=', '').split(/ +/).slice(0, -1)
        token.name = ids[0]
        token.argv = ids.slice(1)
        token.body = parseIndent(token, parseTop())
        return token
      case 'struct':
        const fields = token.code.split('\n').map(x => x.trim()).filter(x => x)
        token.name = fields[0].split(' ')[0]
        token.fields = fields.slice(1).map(x => x.split(' ')[0]).join(',')
        token.type = fields.slice(1).map(x => x.split(' ')[1]).join(',')
        return token
      case 'enum':
        const tags = token.code.split('\n').map(x => x.trim()).filter(x => x)
        token.name = tags[0].split(' ')[0]
        token.enums = tags.slice(1).map(line => {
          const at = line.indexOf(' ')
          let id = line.slice(0, at)
          if (id.endsWith(':')) {
            id = id.slice(0, -1)
            const fields = line.slice(at).trim().split(/ *, */).map(x => x.split(' ')[0].trim()).join(',')
            const types = line.slice(at).trim().split(/ *, */).map(x => x.split(' ')[1].trim()).join(',')
            return {id, fields, types}
          } else {
            return {id}
          }
        })
        return token
      case 'id': parseCall(token); return token
      case 'la': token.ary = until(t => t.tag !== 'ra'); return token
      case 'lp': token.items = until(t => t.tag !== 'rp'); return token
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
      if (next.code === '=>') {
        next.args = token.tag === 'lp' ? token.items.map(x => x.code).join(',') : token.code
      }
      if (next.code === '->') {
        next.else = parseTop()
      }
      return parseLeft(next)
    } else if (next.tag === 'prop') {
      ++pos
      next.target = token
      parseCall(next)
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
    if (node.tag === 'ra') { throw new Error('Invalid ' + str({node,tokens})) }
    if (node.tag === 'rp') { throw new Error('Invalid ' + str({node,tokens})) }
  }

  return nodes
}
function generate(defs) {
  function genCall(argv) {
    return argv.length === 0 ? '' : '(' + argv.map(gen).join(',') + ')'
  }
  function genStruct(token) {
    return '(' + token.fields + ') => ({' + token.fields + '})'
  }
  function genEnum(token) {
    const defs = ['(x, ...args) => args[x.index](x.val)']
    for (const [index, item] of token.enums.entries()) {
      if (item.fields) {
        defs.push(token.name + '.' + item.id + ' = (' + item.fields + ') => ({val:{' + item.fields + '},index:' + index + '})')
      } else {
        defs.push(token.name + '.' + item.id + ' = val => ({val,index:' + index + '})')
      }
    }
    return defs.join('\n')
  }
  function genFunc(token) {
    return (token.argv.length > 0 ? '(' + token.argv.join(',') + ') => ' : '') + gen(token.body)
  }
  function genLine(token) {
    return gen(token)
  }
  function genLines(lines) {
    const body = lines.map(genLine).map((line, i) => (i===lines.length-1) ? 'return ' + line : line).join('\n  ')
    return '(function () {\n  ' + body + '\n})()'
  }
  function gen(token) {
    if (token.lines) {
      return genLines(token.lines)
    }
    switch (token.tag) {
      case 'num':
      case 'str': return token.code
      case 'func': return 'const ' + token.name + ' = ' + genFunc(token)
      case 'struct': return 'const ' + token.name + ' = ' + genStruct(token)
      case 'enum': return 'const ' + token.name + ' = ' + genEnum(token)
      case 'id': return token.code + genCall(token.argv)
      case 'la': return '[' + token.ary.map(gen).join(',') + ']'
      case 'lp': return '(' + token.items.map(gen).join('') + ')'
      case 'prop': return gen(token.target) + token.code + genCall(token.argv)
      case 'op2':
        switch (token.code) {
          case '=': return 'const ' + gen(token.lhs) + token.code + gen(token.rhs)
          case ':=': return 'let ' + gen(token.lhs) + ' = ' + gen(token.rhs)
          case '=>': return '((' + token.args + ') => ' + gen(token.rhs) + ')'
          case '->': return gen(token.lhs) + ' ? ' + gen(token.rhs) + ' : ' + gen(token.else)
          default: return gen(token.lhs) + token.code + gen(token.rhs)
        }
      default: throw new Error('gen ' + str(token))
    }
  }
  return defs.map(gen).join("\n")
}
function infer(defs,tokens) {
  return
  const d = {}
  const types = {
    'num': {},
    'str': {},
  }
  for (const def of defs) {
    if (def.tag !== 'func' && def.tag !== 'struct' && def.tag !== 'enum') { throw new Error('infer ' + str(def)) }
    d[def.name] = def
  }
  function inferCall(token) {
    if (token.code === 'true' || token.code === 'false') { return 'true' }
    token.argv.map(inferAssign)
    const t = d[token.code]
    if (!t) { throw new Error('inferCall ' + str({token,d})) }
    return inferAssign(t)
  }
  function inferArray(token) {
    if (token.ary.length === 0) {
      return 'array'
    } else {
      return token.ary.map(inferAssign)[0]
    }
  }
  function inferProp(token) {
    token.argv.map(inferAssign)
    const target = token.target
    inferAssign(target)
    const name = target.type
    const type = types[name]
    const prop = type && type[token.code]
    if (!type || !prop) { throw new Error('inferProp ' + str({target,name,type,prop})) }
    return prop
  }
  function makeStruct(token) {
    const d = {}
    for (const field of token.fields) {
      d[field] = field
    }
    return d
  }
  function makeEnum(token) {
    const d = {}
    for (const tag of token.enums) {
      d[tag.id] = tag.fields ? makeStruct(tag) : tag.id
    }
  }
  function inferType(token) {
    switch (token.tag) {
      case 'num':
      case 'str': return token.type = token.tag
      case 'func': return inferAssign(token.body)
      case 'struct': return types[token.name] = makeStruct(token)
      case 'enum': return types[token.name] = makeEnum(token)
      case 'id': return inferCall(token)
      case 'la': return inferArray(token)
      case 'lp': return token.items.map(inferAssign)[0]
      case 'prop': return inferProp(token)
      case 'op2':
        switch (token.code) {
          case '=':
          case ':=': return token.lhs.type = inferAssign(token.rhs)
          case '->': return token.lhs.type =  'bool', inferAssign(token.rhs), inferAssign(token.else)
          case '+':
          case '-':
          case '*': inferAssign(token.lhs); inferAssign(token.rhs); return 'num'
          case '>':
          case '<':
          case '>=':
          case '<=':
          case '==':
          case '!=': inferAssign(token.lhs); inferAssign(token.rhs); return 'bool'
          case '=>': return args => env.local(args, token.argv, token.body)
          default: throw new Error('inferType ' + str(token))
        }
      default: throw new Error('inferType ' + str(token))
    }
  }
  function inferAssign(token) {
    return token.type = inferType(token)
  }
  inferAssign(d.main)
  assertInfer(tokens)
}
function assertInfer(tokens) {
  const ts = tokens.filter(t => t.tag !== 'ra' && t.tag !== 'rp')
  for (const t of ts) {
    if (!t.type) {
      throw new Error('assertInfer ' + str({t,ts}))
    }
  }
}
function compile(src) {
  const tokens = tokenize(src)
  const defs = parse(tokens)
  infer(defs)
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
  function eq(expect, main, ...funcs) {
    const src = funcs.map(x => x + '\n').join('') + 'main = ' + main
    const info = compile(src)
    const js = info.js + '\nreturn main'
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
  eq(7, '1 + 2 * 3')
  eq(9, '(1 + 2) * 3')
  eq(5, '1 * (2 + 3)')

  // type
  eq({a:1, b:true}, 'ab(1 true)', 'ab struct:\n  a int\n  b bool')
  eq(1, 'ast(ast.int(1) x=>x y=>y)', 'ast enum:\n  int int\n  add: lhs ast, rhs ast')
  eq(3, 'eval(ast.add(ast.int(1) ast.int(2)))', 'eval a = ast(a x=>x y=>eval(y.lhs)+eval(y.rhs))', 'ast enum:\n  int int\n  add: lhs ast, rhs ast')

  // function
  eq(3, 'add(1 2)', 'add a b = a + b')
  eq(3, 'add(1 2)', 'add = (a b) => a + b')

  // branch
  eq(1, 'a -> b\n  c', 'a = true', 'b = 1', 'c = 2')
  eq(2, 'a -> b\n  c', 'a = false', 'b = 1', 'c = 2')
  eq(2, 'a -> b\n  c -> d\n  e', 'a = false', 'b = 1', 'c = true', 'd = 2', 'e = 3')

  // effect
  eq(1, '\n  count := 0\n  count += 1\n  count')

  // spiteful tests
  eq(1, ' 1 ')
  eq(1, ' ( ( ( 1 ) ) ) ')

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
