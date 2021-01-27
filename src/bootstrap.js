'use strict'
const runtime = require('fs').readFileSync('runtime.js', 'utf8')
const moa = require('fs').readFileSync('moa.moa', 'utf8')

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
function dict2(list) {
  const d = {}
  for (let i=1; i<list.length; i+=2) {
    d[list[i-1]] = list[i]
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
  const lines = src.split('\n')
  function Token(tag,code) {
    this.tag = tag
    this.code = code
  }
  function Liner() {
    this.line = 1
    this.column = 1
  }
  Liner.prototype.mark = function(fragment) {
    const srcLine = lines[this.line-1]
    const col = this.column - 1
    const left = srcLine.slice(0, col)
    const mid = srcLine.slice(col, col + fragment.length)
    const right = srcLine.slice(col + fragment.length)
    return this.line + ': ' + left + '【' + mid + '】' + right
  }
  Liner.prototype.mention = function(text) {
    return '\n' + this.line + ': ' + lines[this.line-1] +
      '\n' + ' '.repeat(this.line.toString().length + this.column + 1) + '^ ' + text
  }
  Liner.prototype.forward = function(fragment) {
    const tokenLines = fragment.split('\n')
    if (tokenLines.length === 1) {
      this.column += fragment.length
    } else {
      this.line += tokenLines.length - 1
      this.column = tokenLines[tokenLines.length-1].length + 1
    }
  }
  const consume = (tag,m) => m ? new Token(tag, typeof(m) === 'string' ? m : m[0]) : null
  const reg = (p,tag,r) => consume(tag, src.slice(p).match(r))
  const some = (p,tag,s) => consume(tag, s.split(' ').find(w => src.slice(p).startsWith(w)))
  const eat = p =>
    reg(p, 'func', /^[A-Za-z_][A-Za-z0-9_]*( +[A-Za-z_][A-Za-z0-9_]*)* +=/) ||
    reg(p, 'struct', /^[A-Za-z_][A-Za-z0-9_]*:(\n  [a-z].*)+/) ||
    reg(p, 'enums', /^[A-Za-z_][A-Za-z0-9_]*\|(\n  [a-z].*)+/) ||
    reg(p, 'int', /^[0-9]+(\.[0-9]+)?/) ||
    reg(p, 'id', /^[A-Za-z_][A-Za-z0-9_]*(,[A-Za-z_][A-Za-z0-9_]*)*\(?/) ||
    reg(p, 'string', /^"(?:(?:\\")|[^"])*"/) ||
    reg(p, 'prop', /^\.[A-Za-z_][A-Za-z0-9_]*\(?/) ||
    reg(p, 'spaces', /^[ \n]+/) ||
    reg(p, 'comment', /^ *#.*/) ||
    some(p, 'la', '[') ||
    reg(p, 'ra', /\][A-Za-z_][A-Za-z0-9_]*/) ||
    some(p, 'ra', ']') ||
    some(p, 'lp', '(') ||
    some(p, 'rp', ')') ||
    some(p, 'op2', '+= -= *= /= || && == != >= <= ++ => := : <- -> > < + - * /')

  const liner = new Liner()
  let indent = 0
  let pos = 0
  let tokens=[]
  while (pos < src.length) {
    const token = eat(pos)
    if (!token) { throw new Error(liner.mention('Tokenize failed')) }
    if (token.tag === 'spaces' && token.code.includes('\n')) {
      const last = token.code.split('\n').slice(-1)[0]
      if (!last.includes('#')) {
        indent = last.length
        if (indent % 2 != 0) { throw new Error('invalid indent=' + indent + ' at ' + token.line) }
      }
    }
    token.indent = indent
    pos += token.code.length
    token.line = liner.mark(token.code)
    liner.forward(token.code)
    tokens.push(token)
  }

  const dst = tokens.map(t => t.code).join('')
  if (src !== dst) throw new Error('tokenize assertion: src=' + str({src,dst}))
  return tokens.filter(t => t.tag !== 'spaces' && t.tag !== 'comment')
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
    return [ary, t]
  }
  function parseCall(token) {
    token.name = escape(token.code.replace('(', '')).replace('.', '')
    if (token.code.endsWith('(')) {
      token.argv = until(t => t.tag !== 'rp')[0]
    } else {
      token.argv = []
    }
  }
  function parseTop() {
    return parseLeft(parseUnit())
  }
  function parseIndent(t1, t2) {
    if (t1.indent < t2.indent) {
      const lines = [copy(t2)]
      while (pos < tokens.length) {
        if (tokens[pos].indent >= t2.indent) {
          lines.push(parseTop())
        } else {
          break
        }
      }
      t2.lines = lines
    }
    return t2
  }
  function parseUnit() {
    const token = tokens[pos++]
    switch (token.tag) {
      case 'int': token.val = token.code; return token
      case 'string': token.val = token.code.slice(1,-1); return token
      case 'ra':
      case 'rp': return token
      case 'func':
        [token.name, ...token.argv] = token.code.replace('=', '').split(/ +/).slice(0, -1)
        token.body = parseIndent(token, parseTop())
        return token
      case 'enums':
        const [ename, ...fields] = token.code.split('\n').map(x => x.trim()).filter(x => x)
        token.name = token.type = ename.replace('|', '')
        token.enums = fields.map(field => {
          const [id, ...bodies] = field.split(/ *[ ,:] */)
          if (bodies.length === 1) {
            return {id, type: bodies[0]}
          } else {
            return {id, struct: dict2(bodies)}
          }
        })
        return token
      case 'struct':
        const [sname, ...struct] = token.code.split('\n').map(x => x.trim()).filter(x => x)
        token.name = token.type = sname.replace(':', '')
        token.struct = struct.map(x => x.split(' '))
        return token
      case 'id': parseCall(token); return token
      case 'la': [token.ary,token.ra] = until(t => t.tag !== 'ra'); return token
      case 'lp': token.items = until(t => t.tag !== 'rp')[0]; return token
      default: throw new Error('Unexpected tag ' + str(token) + info())
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
  const expect = tokens.filter(t => ['func', 'struct', 'enums'].includes(t.tag) && t.indent === 0).map(t => t.name).join(' ')
  const actual = nodes.map(t => t.name).join(' ')
  if (expect !== actual) {
    throw new Error('Lack of information after parsing: ' + str({expect,actual}))
  }

  return nodes
}
function generate(defs) {
  const embeddedProps = {
    'int': {string: 'toString()'},
  }
  const embeddedFuncs = {
    'string': {int: '__stringInt', char: '__stringChar'},
    'eff': {alt: '__alt', then: '__then'},
  }
  const embeddedIds = {
    guard: '__guard',
    match: '__match',
    trace: 'console.log',
    warn: 'console.warn',
  }
  function genCall(argv) {
    return argv.length === 0 ? '' : '(' + argv.map(gen).join(',') + ')'
  }
  function genStruct(token) {
    const fields = token.struct.map(x => x[0])
    return 'const ' + token.name + ' = (' + fields + ') => ({' + fields + '})'
  }
  function genEnum(token) {
    return token.enums.map(x => 'const ' + x.id + " = __val => ({__val,__type:'" + x.id + "'})").join('\n') +
      '\nconst ' + token.name + ' = {' + token.enums.map(x => x.id).join(',') + '}'
  }
  function genFunc(token) {
    const body = gen(token.body)
    if (token.argv.length > 0) {
      return '(' + token.argv.join(',') + ') => ' + body
    } else {
      return body
    }
  }
  function genId(token) {
    if (token.name === 'typeof') {
      return "'" + token.argv[0].type + "'/* typeof " + gen(token.argv[0]) + ' */'
    } else {
      return (embeddedIds[token.name] || token.name) + genCall(token.argv)
    }
  }
  function genProp(token) {
    if (!token.target.type) {
      throw new Error('genProp error: type is undefined ' + str(token))
    }
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
  function genArrow(lhs, rhs) {
    if (lhs.name === '_') {
      return '() => ' + gen(rhs)
    }
    return '() => (' + gen(lhs) + ') ? ' + gen(rhs) + ' : undefined'
  }
  function genTypeMatch(lhs, rhs) {
    if (lhs.tag !== 'id' || rhs.code !== '->' || rhs.lhs.tag !== 'id') {
      throw new Error('Match Syntax Error')
    }
    const name = lhs.name
    const type = rhs.lhs.name
    return '() => ' + name + ".__type === '" + type + "' ? (" + name + '=>' + gen(rhs.rhs) + ')(' + name + '.__val) : undefined'
  }
  function genLine(token) {
    let js = gen(token)
    if (token.type === 'eff' && token.tag === 'id') {
      js = '__e = ' + js + '(); if (__e.__failed) { return __e }'
    }
    return js
  }
  function genLines(token) {
    const body = token.lines.map(genLine).map((line, i) => (i===token.lines.length-1) ? 'return ' + line : line).join('\n  ')
    return '(function () {\n  ' + body + '\n})'
  }
  function gen(token) {
    if (token.lines) {
      return genLines(token)
    }
    switch (token.tag) {
      case 'int': return token.val
      case 'string': return '"' + token.val + '"'
      case 'func': return 'const ' + token.name + ' = ' + genFunc(token)
      case 'struct': return genStruct(token)
      case 'enums': return genEnum(token)
      case 'id': return genId(token)
      case 'la': return '[' + token.ary.map(gen).join(',') + ']'
      case 'lp': return '(' + token.items.map(gen).join('') + ')'
      case 'prop': return genProp(token)
      case 'op2':
        switch (token.op) {
          case '=': return 'const ' + gen(token.lhs) + token.op + gen(token.rhs)
          case ':=': return 'let ' + gen(token.lhs) + ' = ' + gen(token.rhs)
          case '<-': const name = gen(token.lhs); return 'const ' + name + ' = ' + gen(token.rhs) + '(); if(' + name + '.__failed) { return ' + name + ' }'
          case '=>': return '((' + token.args + ') => ' + gen(token.rhs) + ')'
          case '++': return gen(token.lhs) + '.concat(' + gen(token.rhs) + ')'
          case ':': return genTypeMatch(token.lhs, token.rhs)
          case '->': return genArrow(token.lhs, token.rhs)
          default: return gen(token.lhs) + token.op + gen(token.rhs)
        }
      default: throw new Error('gen ' + str(token))
    }
  }
  return defs.map(gen).join("\n")
}
function infer(defs, src, tokens) {
  function f(type, struct) {
    struct ||= {}
    return {type,struct}
  }
  const scope = {
    'true': f('bool'),
    'false': f('bool'),
    'trace': f('eff'),
    'warn': f('eff'),
    'typeof': f('string'),
    'guard': f('eff'),
    'int': f('int', {string: 'string'}),
    'string': f('string', {int: 'eff', char: 'string'}),
    'eff': f('eff', {alt: 'eff', then: 'eff'}),
    'io': f('io', {write: 'void', reads: 'string'}),
  }
  function inferType(token) {
    return token.type || (token.type = _inferType(token))
  }
  function _inferType(token) {
    if (token.lines) {
      inferLines(token.lines)
      return 'eff'
    }
    switch (token.tag) {
      case 'string': return 'string'
      case 'int': return 'int'
      case 'id': return inferId(token)
      case 'prop': return inferProp(token)
      case 'lp': return token.items.map(inferType)[0]
      case 'la': return inferArray(token)
      case 'op2':
        switch (token.op) {
          case '+':
          case '-':
          case '++':
          case '*': return same(token.lhs, token.rhs)
          case '=':
          case ':=':
          case '<-': token.lhs.type = inferType(token.rhs); return 'eff'
          case '+=': should(token.lhs, 'int'); inferType(token.rhs); return should(token.rhs, 'int')
          case '=>': return 'func'
          case '->': return inferPatternMatch(token.lhs, token.rhs)
          default:
            throw new Error('inferType op2 ' + str(token))
        }
      default:
        throw new Error('inferType ' + str(token))
    }
  }
  function inferId(token) {
    if (token.name === 'match') {
      return token.argv.map(x => inferType(x.rhs))[0]
    } else if (token.tag === 'id' && token.name in scope) {
      token.argv.map(inferType)
      const func = scope[token.name]
      const type = func.type
      if (!type) { throw new Error('Not infered yet ' + str({func,token})) }
      if (type === 'func' && token.argv.length) {
        const giveArgs = token.argv.map(t => t.type)
        let defArgs = func.argv.concat(func.body.op === '=>' ? func.body.lhs.items.map(t => t.name) : [])
        if (giveArgs.length !== defArgs.length) {
          throw new Error('Arguments does not match ' + str({token}))
        } else {
          return call(defArgs, giveArgs, () => inferType(func.body))
        }
      } else {
        return type
      }
    } else {
      throw new Error('Not found ' + token.name  + ' ' + str({token}))
    }
  }
  function inferProp(token) {
    const name = token.name
    const type = inferType(token.target)
    if (!type) { throw new Error('Type not infered ' + str({token,scope: Object.keys(scope)})) }
    const prop = scope[type].struct
    if (!prop) { throw new Error('Object not found ' + str({type,name,token})) }
    const ptype = prop[name]
    if (!ptype) { throw new Error('Property not found ' + str({type,name,token})) }
    should(token, ptype)
    return ptype
  }
  function inferArray(token) {
    const t1 = token.ary.map(inferType)[0]
    const t2 = token.ra.code.slice(1)
    const t = t1 || t2
    if (!t) { throw new Error('Array should be typed: ' + str(token)) }
    return '[]' + t
  }
  function inferLines(lines) {
    if (lines.length === 0) {
      throw new Error('inferLines Error: empty lines')
    } else if (lines.length === 1) {
      return inferType(lines[0])
    } else {
      const [token, ...rest] = lines
      if ([':=', '<-'].includes(token.op)) {
        token.lhs.type = inferType(token)
        return local(token.lhs.name, token.rhs.type, () => inferLines(rest))
      } else if (token.tag === 'func') {
        token.type = inferType(token.body)
        return local(token.name, token.type, () => inferLines(rest))
      } else {
        inferType(token)
        return inferLines(rest)
      }
    }
  }
  function inferPatternMatch(lhs, rhs) {
    if (lhs.argv.length) {
      return local(lhs.argv[0].name, lhs.name, () => inferType(rhs))
    } else {
      return inferType(rhs)
    }
  }
  function local(k, v, f) {
    return call([k], [v], f)
  }
  function call(keys, vals, f) {
    if (keys.length !== vals.length) {
      throw new Error('Length does not match: ' + str({keys,vals}))
    }
    if (keys.some(x => x === undefined)) { throw new Error('Invalid keys: ' + str(keys)) }
    if (vals.some(x => x === undefined)) { throw new Error('Invalid vals: ' + str(vals)) }
    const backup = {}
    for (let i=0; i<keys.length; i++) {
      const key = keys[i]
      if (key in scope) {
        backup[key] = scope[key]
      }
      scope[key] = {type: vals[i]}
    }
    const ret = f()
    for (const k in Object.keys(backup)) {
      if (k in scope) {
        scope[k] = backup[k]
      } else {
        delete scope[k]
      }
    }
    return ret
  }
  function should(token, type) {
    if (token.type && token.type !== type) {
      throw new Error('unexpected ' + type + ' != ' + str(token))
    }
    return token.type = type
  }
  function same(...tokens) {
    const type = tokens.map(inferType)[0]
    if (tokens.every(token => token.type === type)) {
      return type
    } else {
      throw new Error('Types should be same ' + str(tokens) + ' in ' + src)
    }
  }

  for (const def of defs) {
    if (def.tag === 'func') {
      def.type = def.argv.length ? 'func' : inferType(def.body)
    } else if (def.enums) {
      for (const e of def.enums) {
        scope[e.id] = e
      }
    }
    scope[def.name] = def
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
  const originalConsoleLog = console.log
  const originalConsoleWarn = console.warn
  try {
    const stdouts = []
    const stderrs = []
    console.log = x => stdouts.push(x), undefined
    console.warn = x => stderrs.push(x), undefined
    const value = Function(runtime + '\n' + js)()
    const stdout = stdouts.join('\n')
    const stderr = stderrs.join('\n')
    return {value,stdout,stderr}
  } catch (e) {
    puts('Failed to evaluate')
    for (const [i, line] of js.split('\n').entries()) {
      put((1 + i).toString().padStart(3) + ':', line)
    }
    return {value: e, stdout: '', stderr: ''}
  } finally {
    console.log = originalConsoleLog
    console.warn = originalConsoleWarn
  }
}
function testAll() {
  unitTests()
  moaTests()

  console.log('ok')
}
function unitTests() {
  function eq(...args) {
    return equals(r => r.value, ...args)
  }
  function stdout(...args) {
    return equals(r => r.stdout, ...args)
  }
  function stderr(...args) {
    return equals(r => r.stderr, ...args)
  }
  function fail(...args) {
    return equals(r => r.value.message, ...args)
  }
  function equals(unwrap, expect, main, ...funcs) {
    const src = funcs.map(x => x + '\n').join('') + 'main = ' + main
    const info = compile(src)
    const js = info.js + "\nreturn typeof(main) === 'function' ? main() : main"
    const result = evalInSandbox(js)
    const actual = unwrap(result)
    if (str(expect) === str(actual)) {
      put('.')
    } else {
      console.error('Failed')
      put('expect: ', expect)
      put('actual: ', actual)
      put('src   : ', src)
      put('js    : ', '\n  ' + info.js.replace(/\n/g, '\n  '))
      put('result: ', result)
      put('defs  : '); puts(...info.defs)
      process.exit(1)
    }
  }

  // basic values
  eq(1, '1')
  eq(true, 'true')
  eq('a', '"a"')
  eq([], '[]int')
  eq([1, 2, 3], '[1 2 3]')

  // expression
  eq(3, '1+2')
  eq(7, '1 + 2 * 3')
  eq(9, '(1 + 2) * 3')
  eq(5, '1 * (2 + 3)')

  // function
  eq(3, 'add(1 2)', 'add a b = a + b')
  eq(3, 'add(1 2)', 'add = (a b) => a + b')

  // type
  eq({a: 1, b: true}, 'ab(1 true)', 'ab:\n  a int\n  b bool')
  eq({ __val: 1, __type: 'a'}, 'a(1)', 'ab|\n  a int\n  b []int')
  eq({ __val: [1], __type: 'b'}, 'b([1])', 'ab|\n  a int\n  b []int')

  // control flow
  eq(1, 'match(a -> b\n  _ -> c)', 'a = true', 'b = 1', 'c = 2')
  eq(2, 'match(a -> b\n  _ -> c)', 'a = false', 'b = 1', 'c = 2')
  eq(2, 'match(a -> b\n  c -> d\n  _ -> e)', 'a = false', 'b = 1', 'c = true', 'd = 2', 'e = 3')

  // pattern match for enum
  eq(3, 'f(a(1)) + f(a(2))', 'ab|\n  a int\n  b bool', 'f v = match(v:a->v v:b->v)')
  eq(true, 'f(b(true))', 'ab|\n  a int\n  b bool', 'f v = match(v:a->v v:b->v)')

  // effect
  eq(3, '\n  count := 0\n  count += 1\n  count += 2\n  count')
  eq(2, 'f', 'f =\n  n := 0\n  g =\n    n += 1\n    n\n  g\n  g')
  fail('string.int: not a number hi', '"hi".int')
  eq(0, '"a".int.alt(0)')
  eq(1, '"1".int.alt(0)')
  eq(3, '"1".int.then((x) => x + 2)')
  eq(0, '"a".int.then((x) => x + 2).alt(0)')

  // type inference
  eq(1, '"1".int')
  eq('1', 'a.string', 'a = 1')
  eq('2', '(a + 1).string', 'a = 1')
  eq('4c', '(a + b + 1).string ++ c', 'a = 1', 'b = 2', 'c = "c"')
  eq('[]int', 'typeof([]int)')

  // embedded
  stdout('1', 'trace(1)')
  stdout('hi', 'trace("hi")')
  stderr('hi', 'warn("hi")')
  eq('1', '1.string')
  eq('i', '"hi".char(1)')
  fail('out of index', '"hi".char(2)')
  eq(1, '\n  guard(true)\n  1')
  fail('guard', '\n  guard(false)\n  1')

  // embedded typeof
  eq('int', 'typeof(1)')
  eq('string', 'typeof("hi")')
  eq('int', 'typeof(f)', 'f = 1')
  eq('func', 'typeof(f)', 'f a = a')
  eq('int', 'typeof(g)', 'f = 1', 'g = f')
  eq('func', 'typeof(g)', 'f a = 1', 'g = f')
  eq('int', 'typeof(match(true -> 1))')
  eq('int', 'typeof(match(f:a -> f))', 'ab|\n  a  int\n  b  bool', 'f = a(1)')
  eq('bool', 'typeof(match(f:a -> f))', 'ab|\n  a  int\n  b  bool', 'f = b(true)')
  eq('string', 'typeof(match(f:a -> "a"))', 'ab|\n  a  int\n  b  bool', 'f = b(true)')
  eq('eff', 'typeof(f)', 'f = trace(1)')
  eq('eff', 'typeof(f)', 'f =\n  n:=0\n  n')
  eq('int', 'f', 'f =\n  n:=0\n  typeof(n)')

  // spiteful tests
  eq(1, ' 1 ')
  eq(1, ' ( ( ( 1 ) ) ) ')
}
function moaTests() {
  const moaJs = compile(moa).js
  function eq(expect, src) {
    const js = [
      'let __stdout=""',
      'io={reads: () => ' + str(src) + ', write: s=>__stdout+=s}',
      moaJs,
      'main()',
      'return __stdout'].join('\n')
    const result = evalInSandbox(js)
    const actual = result.value
    if (str(expect) === str(actual)) {
      put('.')
    } else {
      console.error('Failed')
      put('expect: ', expect)
      put('actual: ', actual)
      put('result: ', result)
      put('src   : ', src)
      put('js    : ', '\n  ' + js.replace(/\n/g, '\n  '))
      process.exit(1)
    }
  }

  eq('1', '1')
}
function compileStdin() {
  const moa = require('fs').readFileSync('/dev/stdin', 'utf8')
  const js = compile(moa).js
  console.log(js)
}
function main() {
  process.argv[2] === 'test' ? testAll() : compileStdin()
}
main()
