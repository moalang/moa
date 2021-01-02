'use strict'

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
function copy(obj) {
  return JSON.parse(JSON.stringify(obj))
}
function zip(keys, vals) {
  const len = Math.min(keys.length, vals.length)
  const ary = []
  for (let i=0; i<len; ++i) {
    ary.push([keys[i], vals[i]])
  }
  return ary
}
function dict(kvs) {
  const d = {}
  for (const [k,v,..._] of kvs) {
    if (k in d) {
      throw new Error('Duplicated key=' + k + str(kvs))
    }
    d[k] = v
  }
  return d
}
function dig(d, ...args) {
  for (const arg of args) {
    d = d[arg]
    if (d === undefined) {
      return undefined
    }
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
    token.name = escape(token.code.replace('(', '')).replace('.', '')
    if (token.code.endsWith('(')) {
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
      case 'num': token.val = token.code; return token
      case 'str': token.val = token.code.slice(1,-1); return token
      case 'ra':
      case 'rp': return token
      case 'func':
        const ids = token.code.replace('=', '').split(/ +/).slice(0, -1)
        token.name = ids[0]
        token.argv = ids.slice(1)
        token.body = parseIndent(token, parseTop())
        return token
      case 'struct':
        const [name, ...fields] = token.code.split('\n').map(x => x.trim()).filter(x => x)
        token.name = name.split(' ')[0]
        token.struct = dict(fields.map(x => x.split(' ')))
        return token
      case 'enum':
        const tags = token.code.split('\n').map(x => x.trim()).filter(x => x)
        token.name = tags[0].split(' ')[0]
        token.enum = tags.slice(1).map(line => {
          const at = line.indexOf(' ')
          let id = line.slice(0, at)
          if (id.endsWith(':')) {
            id = id.slice(0, -1)
            const struct = dict(line.slice(at).trim().split(/ *, */).map(x => x.split(' ').map(y => y.trim())))
            return {id, struct}
          } else {
            return {id, alias: line.slice(at)}
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
      next.op = next.code
      next.lhs = token
      next.rhs = parseTop()
      if (next.op === '=>') {
        next.args = token.tag === 'lp' ? token.items.map(x => x.name).join(',') : token.name
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
  const embeddedProps = {
    'num': {string: 'toString()'}
  }
  const embeddedFuncs = {
    'str': {int: 'parseInt'}
  }
  function genCall(argv) {
    return argv.length === 0 ? '' : '(' + argv.map(gen).join(',') + ')'
  }
  function genStruct(token) {
    const fields = Object.keys(token.struct).join(',')
    return '(' + fields + ') => ({' + fields + '})'
  }
  function genEnum(token) {
    const enums = []
    for (const [index, item] of token.enum.entries()) {
      if (item.struct) {
        const fields = Object.keys(item.struct).join(',')
        enums[index] = item.id + ': (' + fields + ') => ({switch: (...args) => args[' + index + ']({' + fields + '})})'
      } else {
        enums[index] = item.id + ': (v) => ({switch: (...args) => args[' + index + '](v)})'
      }
    }
    return '{\n  ' + enums.join(',\n  ') + '\n}'
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
  function genId(token) {
    if (token.name === 'if') {
      return token.argv.filter(t => t.code === '->').map(x => gen(x.lhs) + '?' + gen(x.rhs) + ':').join(' ') + gen(token.argv.slice(-1)[0])
    } else {
      return token.name + genCall(token.argv)
    }
  }
  function genProp(token) {
    const prop = dig(embeddedProps, token.target.type, token.name)
    if (prop) {
      return wrapIfNum(gen(token.target)) + '.' + prop + genCall(token.argv)
    }
    const func = dig(embeddedFuncs, token.target.type, token.name)
    if (func) {
      return func + genCall([token.target].concat(token.argv))
    }
    return wrapIfNum(gen(token.target)) + '.' + token.name + genCall(token.argv)
  }
  function wrapIfNum(s) {
    return parseInt(s).toString() === s ? '(' + s + ')' : s
  }
  function gen(token) {
    if (token.lines) {
      return genLines(token.lines)
    }
    switch (token.tag) {
      case 'num': return token.val
      case 'str': return '"' + token.val + '"'
      case 'func': return 'const ' + token.name + ' = ' + genFunc(token)
      case 'struct': return 'const ' + token.name + ' = ' + genStruct(token)
      case 'enum': return 'const ' + token.name + ' = ' + genEnum(token)
      case 'id': return genId(token)
      case 'la': return '[' + token.ary.map(gen).join(',') + ']'
      case 'lp': return '(' + token.items.map(gen).join('') + ')'
      case 'prop': return genProp(token)
      case 'op2':
        switch (token.op) {
          case '=': return 'const ' + gen(token.lhs) + token.op + gen(token.rhs)
          case ':=': return 'let ' + gen(token.lhs) + ' = ' + gen(token.rhs)
          case '=>': return '((' + token.args + ') => ' + gen(token.rhs) + ')'
          case '->': throw new Error('gen -> ' + str(token))
          default: return gen(token.lhs) + token.op + gen(token.rhs)
        }
      default: throw new Error('gen ' + str(token))
    }
  }
  return defs.map(gen).join("\n")
}
function infer(defs, src, tokens) {
  const tenv = {
    'num': {'string': 'string'},
  }
  function inferType(token) {
    return token.type = _inferType(token)
  }
  function _inferType(token) {
    switch (token.tag) {
      case 'str':
      case 'num': return token.tag
      case 'id': return tenv[token.name]
      case 'prop': return tenv[inferType(token.target)]
      case 'lp': return token.items.map(inferType)[0]
      case 'la': return token.ary.map(inferType)[0] || 'tvar'
      case 'op2':
        switch (token.op) {
          case '+':
          case '-':
          case '*':
          case ':=': return unify(token.lhs, token.rhs)
          case '=>': return inferType(token.rhs)
          default:
            throw new Error('inferType op2 ' + str(token))
        }
      default:
        throw new Error('inferType ' + str(token))
    }
  }
  function unify(t1, t2) {
    if (t1 == t2) {
      return t1
    } else if ((t1 && !t2) || (!t1 && t2)) {
      return t1 || t2
    } else {
      return unify(inferType(t1), inferType(t2))
      throw new Error('unify failed ' + str(t1) + ' and ' + str(t2))
    }
  }
  for (const def of defs) {
    if (def.tag === 'func' && def.argv.length === 0) {
      tenv[def.name] = inferType(def.body)
    }
  }
  inferType(defs.filter(x => x.name === 'main')[0].body)
}
function compile(src) {
  const tokens = tokenize(src)
  let defs = parse(tokens)
  infer(defs, src, tokens)
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
  eq(1, 'ast.int(1).switch(x=>x y=>y)', 'ast enum:\n  int int\n  add: lhs ast, rhs ast')
  eq(3, 'eval(ast.add(ast.int(1) ast.int(2)))', 'eval a = a.switch(x=>x y=>eval(y.lhs)+eval(y.rhs))', 'ast enum:\n  int int\n  add: lhs ast, rhs ast')

  // function
  eq(3, 'add(1 2)', 'add a b = a + b')
  eq(3, 'add(1 2)', 'add = (a b) => a + b')

  // branch
  eq(1, 'if(a -> b\n  c)', 'a = true', 'b = 1', 'c = 2')
  eq(2, 'if(a -> b\n  c)', 'a = false', 'b = 1', 'c = 2')
  eq(2, 'if(a -> b\n  c -> d\n  e)', 'a = false', 'b = 1', 'c = true', 'd = 2', 'e = 3')

  // effect
  eq(1, '\n  count := 0\n  count += 1\n  count')

  // type inference
  eq('1', '1.string')
  eq(1, '"1".int')
  eq('1', 'a.string', 'a = 1')

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
