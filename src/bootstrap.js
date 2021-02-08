'use strict'
//const moa = require('fs').readFileSync('moa.moa', 'utf8')

// utils
const str = obj => JSON.stringify(obj, null, 2)
const put = s => process.stdout.write(typeof s === 'string' ? s : str(s))
const print = (...a) => console.log(...a)
const warn = (...a) => console.warn(...a)
const dump = o => (console.dir(o, {depth: null}), o)
const copy = o => JSON.parse(JSON.stringify(o))
const dig = (d,...args) => args.reduce((o,name) => o[name], d)
const range = (s,e,n=1) => {
  if (e === undefined) { e=s; s=0 }
  const l = []
  for (let i=s; i<e; i+=n) {
    l.push(i)
  }
  return l
}
const twin = (f,a,n=1) => range(n, a.length, 2).map(i => f(a[i-1], a[i]))
const elipsis = s => s.length <= 100 ? s : s.slice(0, 96) + ' ...\n'
const cat = s => {
  const lines = s.split('\n')
  const l = lines.length.toString().length
  for (const [i,line] of lines.entries()) {
    const no = (i+1).toString().padStart(l, ' ')
    print(no + ': ' + line)
  }
}

// runtime helper functions
function Failure(message) { this.message = message }
Failure.prototype.__failed = true
Failure.prototype.then = function() { return this }
Failure.prototype.alt = (...args) => args.first(a => a)
Object.prototype.then = function(f) { return f(this) }
Object.prototype.alt = function() { return this }
Function.prototype.then = function(f) { return () => this().then(f) }
Function.prototype.alt = function(...args) { return () => this().alt(...args) }
function extend(obj, d) {
  for (const key of Object.keys(d)) {
    const f = d[key]
    if (f.length <= 1) {
      Object.defineProperty(obj.prototype, key, { get: function() { return f(this) } })
    } else {
      obj.prototype[key] = function(...args) { return f(this, ...args) }
    }
  }
  Object.defineProperty(obj.prototype, 'string', { get: function() { return str(this) } })
}
const ooo = new Failure('out of index')
extend(Number, {
  char: n => String.fromCharCode(n),
})
extend(String, {
  at: (s,n) => s[n] || ooo,
  count: s => s.length,
  int: s => s.match(/^[0-9]+$/) ? parseInt(s) : new Failure('string.int: not a number ' + s),
})
extend(Array, {
  at: (a, n) => n < a.length && 0 <= n ? a[n] : ooo,
  all: (a, f) => a.every(f),
  present: a => !!a.length,
  count: a => a.length,
  first: (a, f) => {
    let ret = new Failure('empty')
    for (const e of a) {
      ret = __eff(f(e))
      if (ret.__failed) {
        continue
      }
      return ret
    }
    return ret
  },
  head: a => a.length ? a[0] : ooo,
  last: a => a.length ? a[a.length-1] : ooo,
})
global.__failure = message => new Failure(message)
global.__eff = o => typeof o === 'function' ? __eff(o()) : o
global.__equals = (a,b) => a === b || str(a) === str(b)
global.__dump = dump
global.assert = (cond,message,...params) => {
  if (cond) { return }
  throw new Error('Assert: ' + message + (params.length ? ' ' + str(params) : ''))
}
function compile(src) {
  function tokenize() {
    function Token(tag, code, pos) {
      this.tag = tag
      this.code = code
      this.pos = pos
    }
    const consume = (p,tag,m) => m ? new Token(tag, typeof(m) === 'string' ? m : m[0], p) : null
    const reg = (p,tag,r) => consume(p, tag, src.slice(p).match(r))
    const some = (p,tag,s) => consume(p, tag, s.split(' ').find(w => src.slice(p).startsWith(w)))
    const eat = p =>
      reg(p, 'func', /^[a-z_][a-z0-9_]*( +[a-z_][a-z0-9_]*)* +=(?![>=])/) ||
      reg(p, 'struct', /^[A-Za-z_][A-Za-z0-9_]*:(\n  [a-z].*)+/) ||
      reg(p, 'adt', /^[A-Za-z_][A-Za-z0-9_]*\|(\n  [a-z].*)+/) ||
      reg(p, 'int', /^[0-9]+/) ||
      reg(p, 'id', /^[a-z_][a-z0-9_]*/) ||
      reg(p, 'string', /^"(?:(?:\\")|[^"])*"/) ||
      reg(p, 'string', /^`(?:(?:\\`)|[^`])*`/) ||
      reg(p, 'prop', /^\.[a-z_][a-z0-9_]*/) ||
      reg(p, 'spaces', /^[ \n]+/) ||
      reg(p, 'comment', /^ *#.*/) ||
      some(p, 'la', '[') ||
      some(p, 'ra', ']') ||
      some(p, 'lp', '(') ||
      some(p, 'rp', ')') ||
      some(p, 'op2', '+= -= *= /= || && == != >= <= ++ => :: := <- > < + - * /')

    const liner = (function() {
      const lines = src.split('\n')
      let line = 1
      let column = 1
      return {
        at: () => `${line}: ${lines[line-1]}`,
        mention: t => `\n${line}: ${lines[line-1]}\n` +
          ' '.repeat(line.toString().length + column) + ' ^' + t,
        forward: s =>  {
          const l = s.split('\n')
          if (l.length === 1) {
            column += s.length
          } else {
            line += l.length - 1
            column = l[l.length-1].length + 1
          }
        }
      }
    })()
    let indent = 0
    let pos = 0
    let tokens=[]
    let prev = {}
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
      if (token.code === '(' && (prev.tag === 'id' || prev.tag === 'prop')) {
        prev.calling = true
      }
      token.indent = indent
      token.at = liner.at()
      pos += token.code.length
      liner.forward(token.code)
      tokens.push(token)
      prev = token
    }
    const dst = tokens.map(t => t.code).join('')
    if (src !== dst) throw new Error('tokenize assertion: src=' + str({src,dst}))
    return tokens.filter(t => t.tag !== 'spaces' && t.tag !== 'comment')
  }

  function parse(tokens) {
    const nodes = []
    let pos = 0
    const parseTop = () => parseLeft(parseUnit())
    function parseAssert(f, g, expect) {
      const r = f()
      if (!g(r)) { throw new Error(`Expect ${expect} but ${str(r)}`) }
      return r
    }
    function parseCall(token) {
      token.name = escape(token.code.replace('(', '')).replace('.', '')
      token.argv = token.calling ? parseAssert(parseUnit, t => t.code === '(', '(').items : []
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
          [token.name, ...token.args] = token.code.replace('=', '').split(/ +/).slice(0, -1)
          token.body = parseIndent(token, parseTop())
          return token
        case 'adt':
          const [aname, ...lines] = token.code.split('\n').map(x => x.trim()).filter(x => x)
          token.name = token.type = aname.replace('|', '')
          token.adt = lines.map(line => {
            const [tag, kvs] = line.split(':')
            const keys = kvs ? kvs.split(',').map(f => f.trim().split(' ')[0]) : []
            return {tag, keys}
          })
          return token
        case 'struct':
          const [sname, ...kvs] = token.code.split(/[\n: ]/).map(x => x.trim()).filter(x => x)
          token.name = sname
          token.keys = twin((a,_) => a, kvs)
          return token
        case 'id': parseCall(token); return token
        case 'la': token.ary = until(t => t.tag !== 'ra'); return token
        case 'lp': token.items = until(t => t.tag !== 'rp'); return token
        default: throw new Error('Unexpected tag ' + str(token))
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
          next.args = token.tag === 'lp' ? token.items.map(x => x.name) : [token.name]
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
    function until(f) {
      const ary = []
      let t
      while (pos < tokens.length && f(t = parseTop())) {
        ary.push(t)
      }
      return ary
    }

    while (pos < tokens.length) {
      let node = parseTop()
      if (!node) { throw new Error('failed to parse at=' + pos + ' tokens=' + str(tokens)) }
      nodes.push(node)
    }
    for (const node of nodes) {
      if (node.tag === 'op2' && (!node.lhs || !node.rhs)) { throw new Error('Invalid op2 ' + str(node)) }
      if (node.tag === 'prop' && (!node.target || !node.argv)) { throw new Error('Invalid prop ' + str(node)) }
      if (node.tag === 'la' && (!node.ary)) { throw new Error('Invalid (' + str(node)) }
      if (node.tag === 'lp' && (!node.items)) { throw new Error('Invalid )' + str(node)) }
      if (node.tag === 'ra') { throw new Error('Invalid ( ' + str({node,nodes})) }
      if (node.tag === 'rp') { throw new Error('Invalid ) ' + str({node,nodes})) }
    }
    const expect = tokens.filter(t => ['func', 'struct', 'adt'].includes(t.tag) && t.indent === 0).map(t => t.name).join(' ')
    const actual = nodes.map(t => t.name).join(' ')
    if (expect !== actual) {
      throw new Error('Lack of information after parsing: ' + str({expect,actual}))
    }
    return nodes
  }
  function generate(defs) {
    const genStruct = ({name,keys}) => `const ${name} = (${keys}) => ({${keys}})`
    const genAdtElement = ({tag,keys}) => `const ${tag} = ` + (keys.length ? `(${keys}) => ` : '') + `({${keys.concat(['__type'])}:'${tag}'})`
    const genAdt = ({name, adt}) => `${adt.map(genAdtElement).join('\n')}\nconst ${name} = {${adt.map(x => x.tag)}}`
    const genFunc = ({args,body}) => (args.length ? '(' + args + ') => ' : '') + gen(body)
    const genCall = argv => argv.length ? '(' + argv.map(gen) + ')' : ''
    const genProp = ({target,name,argv}) => wrapIfInt(gen(target)) + '.' + name + genCall(argv)
    const wrapIfInt = s => s.match(/^[0-9]/) ? '(' + s + ')' : s
    const genLine = t => t.tag === 'id' && t.name !== 'assert' ? `__e = __eff(${gen(t)}); if (__e.__failed) { return __e }` : gen(t)
    const genLines = ({indent,lines}) => 'function() {\n' + ' '.repeat(indent) + (l => l.slice(0,-1).concat('return ' + l.slice(-1)[0]).join('\n' +  ' '.repeat(indent)))(lines.map(genLine)) + '\n' + ' '.repeat(indent-2) + '}'
    const genMatch = (cond,body) =>
          cond === '_' ? `true ? ${body} : ` :
          cond.match(/^[a-z_]/) ?  `___m.__type === '${cond}' ? ${body} :` :
          `__equals(___m, ${cond}) ? ${body} :`
    function genId(token) {
      if (token.name === 'true' || token.name === 'false' || token.name === '_') {
        return token.name
      } else if (token.name === 'guard') {
        return token.argv.map(gen).join(' && ') + ' ? ({}) : __failure("guard")'
      } else if (token.name === 'assert') {
        const [cond, ...params] = token.argv
        const value = cond.tag === 'op2' ? gen(cond.lhs) + '.string+"'+cond.op+'"+' + gen(cond.rhs) + '.string' : 'false'
        const at = str(cond.at)
        return `assert(${gen(cond)}, ${value} + " at " + ${at}, ${params.map(gen).join(',')})`
      } else if (token.name === 'if') {
        const l = token.argv.length
        if (l%2!=1) { throw new Error('if arguments have to odd number of arguments: ' + str(token)) }
        const argv = token.argv.map(gen)
        return twin((a,b) => `${a} ? ${b} : `, argv).join('') + argv.slice(-1)[0]
      } else if (token.name === 'match') {
        if (token.argv.length % 2 !== 1) { throw new Error('match arguments have to odd number of arguments: ' + str(token)) }
        const argv = token.argv.map(gen)
        const exps = twin(genMatch, argv, 2)
        const at = str(token.at)
        const space = ' '.repeat(token.indent)
        return `(___m => ${exps.map(s => '\n  ' + space + s).join('')}\n  ${space}(()=>{throw new Error("miss match: " + ___m.string + " at " + ${at})})())(${argv[0]})`
      } else {
        return token.name + genCall(token.argv)
      }
    }
    function gen(token) {
      if (token.lines) { return genLines(token) }
      switch (token.tag) {
        case 'int': return token.val
        case 'string': return '"' + token.val.replace('\n', '\\n') + '"'
        case 'func': return 'const ' + token.name + ' = ' + genFunc(token)
        case 'struct': return genStruct(token)
        case 'adt': return genAdt(token)
        case 'id': return genId(token)
        case 'la': return '[' + token.ary.map(gen) + ']'
        case 'lp': return '(' + token.items.map(gen).join('') + ')'
        case 'prop': return genProp(token)
        case 'op2':
          switch (token.op) {
            case '=': return 'const ' + gen(token.lhs) + token.op + gen(token.rhs)
            case '::': return 'let ' + gen(token.lhs) + ' = ' + gen(token.rhs)
            case ':=': return gen(token.lhs) + ' = ' + gen(token.rhs)
            case '<-': const name = gen(token.lhs); return 'const ' + name + ' = __eff(' + gen(token.rhs) + '); if(' + name + '.__failed) { return ' + name + ' }'
            case '=>': return '((' + token.args + ') => ' + gen(token.rhs) + ')'
            case '==': return `__equals(${gen(token.lhs)}, ${gen(token.rhs)})`
            default: return gen(token.lhs) + token.op + gen(token.rhs)
          }
        default: throw new Error('gen ' + str(token))
      }
    }
    return defs.map(gen).join("\n")
  }

  const tokens = tokenize()
  const defs = parse(tokens)
  const js = generate(defs)
  try {
    Function(js)()
    return {defs, js}
  } catch (error) {
    return {defs, js, error}
  }
}
function evaluate(src) {
  const ret = compile(src)
  if (ret.error) {
    return ret
  }
  try {
    ret.value = Function(ret.js + '\nreturn __eff(main)')()
  } catch (e) {
    ret.error = e
  }
  return ret
}
function runTest() {
  testBootstrap()
  testMoa()
}
function testBootstrap() {
  function equals(unwrap, expect, main, ...funcs) {
    const src = funcs.map(x => x + '\n').join('') + 'main = ' + main
    const result = evaluate(src)
    const actual = unwrap(result)
    if (str(expect) === str(actual)) {
      put('.')
    } else {
      warn('Failed')
      print('expect: ', expect)
      print('actual: ', actual)
      print('src   : ', src)
      print('dump  : ')
      dump(result)
      process.exit(1)
    }
  }
  const eq = (...args) => equals(r => r.value, ...args)
  const fail = (...args) => equals(r => r.value && r.value.message, ...args)

  // basic values
  eq(1, '1')
  eq(true, 'true')
  eq('a', '"a"')
  eq('a', '`a`')
  eq('a\nb', '`a\nb`')
  eq([], '[]')
  eq([1, 2], '[1 2]')

  // expression
  eq(3, '1+2')
  eq(7, '1 + 2 * 3')
  eq(9, '(1 + 2) * 3')
  eq(5, '1 * (2 + 3)')
  eq(true, '1 < 2')

  // function
  eq(3, 'add(1 2)', 'add a b = a + b')

  // type
  eq({a: 1, b: true}, 'ab(1 true)', 'ab:\n  a int\n  b bool')
  eq(true, 'ab(1 true) == ab(1 true)', 'ab:\n  a int\n  b bool')
  eq(false, 'ab(1 true) == ab(2 true)', 'ab:\n  a int\n  b bool')
  eq({x: 1, __type: 'a'}, 'a(1)', 'adt|\n  a: x int\n  b: y []int')
  eq({y: [1], __type: 'b'}, 'b([1])', 'adt|\n  a: x int\n  b: y []int')

  // control flow
  eq(1, 'if(true 1 2)')
  eq(2, 'if(false 1 2)')
  eq(3, 'if(false 1 false 2 3)')
  eq(1, 'if(true 1 not_found)') // check lazy evaluation
  eq(10, 'match(1 1 10 2 20)')
  eq(20, 'match(2 1 10 2 20)')
  eq(99, 'match(3 1 10 2 20 _ 99)')
  eq(99, 'match(3 1 10 2 20 _ 99 _ 999)')
  eq(10, 'match(1 1 10 2 not_found)') // check lazy evaluation
  eq(1, 'f(a(1))', 'f v = match(v a v.x b v.y)', 'adt|\n  a: x int\n  b: y []int')
  eq([1], 'f(b([1]))', 'f v = match(v a v.x b v.y)', 'adt|\n  a: x int\n  b: y []int')
  eq(1, 'f(a)', 'f v = match(v a 1 b 2)', 'adt|\n  a\n  b')
  eq(2, 'f(b)', 'f v = match(v a 1 b 2)', 'adt|\n  a\n  b')

  // effect
  eq(3, '\n  count :: 0\n  count += 1\n  count += 2\n  count')
  eq(2, 'f', 'f =\n  n :: 0\n  g =\n    n += 1\n    n\n  g\n  g')
  eq(1, 'f', 'f =\n  n :: 0\n  g =\n    n := 1\n    n\n  g\n  n')
  fail('string.int: not a number hi', '"hi".int')
  eq(0, '"a".int.alt(0)')
  eq(1, '"1".int.alt(0)')
  eq(3, '"1".int.then((x) => x + 2)')
  eq(0, '"a".int.then((x) => x + 2).alt(0)')

  // embedded integer
  eq('1', '1.string')
  eq('A', '65.char')

  // embedded string
  eq(2, '"hi".count')
  eq('i', '"hi".at(1)')
  fail('out of index', '"hi".at(2)')

  // embedded array
  eq('[]', '[].string')
  eq(str([1, 2]), '[1 2].string')
  eq(1, '[1].at(0)')
  fail('out of index', '[1].at(1)')
  fail('out of index', '[1].at(0-1)')
  eq([2, 3, 4], '[1 2 3].map(x => x + 1)')
  eq(false, '[].present')
  eq(true, '[1].present')
  eq(1, '[1 2].head')
  eq(2, '[1 2].last')
  fail('out of index', '[].head')
  fail('out of index', '[].last')

  // embedded effect
  eq(1, '\n  guard(true)\n  1')
  fail('guard', '\n  guard(false)\n  1')
  eq(2, 'f.then(a => a+1)', 'f =\n  guard(true)\n  1')
  fail('guard', 'f.then(a => a+1)', 'f =\n  guard(false)\n  1')
  eq(2, 'f.alt(2)', 'f =\n  guard(false)\n  1')

  // spiteful tests
  eq(1, ' 1 ')
  eq(1, ' ( ( ( 1 ) ) ) ')

  print('ok')
}
function testMoa() {
  const moa = require('fs').readFileSync('moa.moa', 'utf8')
  const result = compile(moa)
  if (result.error) {
    print('Failed compile compiler')
    print('error : ', result.error)
    print(result.js)
    process.exit(1)
  }
  const compiler = result.js

  function eq(expect, main, ...funcs) {
    funcs.push('main = ' + main)
    const src = str(funcs.join('\n'))
    try {
      const js = Function(compiler + '\nreturn __eff(compile(' + src + '))')()
      const actual = Function(js + '\nreturn __eff(main)')()
      if (__equals(expect, actual)) {
        put('.')
      } else {
        warn('Failed')
        print('main  : ', elipsis(main))
        print('expect: ', expect)
        print('actual: ', actual)
        cat(compiler)
        process.exit(1)
      }
    } catch (e) {
      warn('Failed')
      print(e)
      print("#", funcs.join("\n"))
      cat(compiler)
      process.exit(1)
    }
  }

  // basic values
  eq(1, '1')
  eq(true, 'true')
  eq('a', '"a"')
  eq('a', '`a`')
  eq('a\nb', '`a\nb`')
  eq([], '[]')
  eq([1, 2], '[1 2]')

  // embedded string
  eq('1', '1.string')
  eq(2, '"hi".count')
  eq('i', '"hi".at(1)')

  // embedded array
  //eq('[]', '[].string')
  //eq(str([1, 2]), '[1 2].string')
  //eq(1, '[1].at(0)')
  //fail('out of index', '[1].at(1)')
  //fail('out of index', '[1].at(0-1)')
  //eq([2, 3, 4], '[1 2 3].map(x => x + 1)')

  // expression
  eq(3, '1+2')
  eq(7, '1 + 2 * 3')
  eq(9, '(1 + 2) * 3')
  eq(5, '1 * (2 + 3)')
  eq(true, '1 < 2')

  // function
  eq(3, 'add(1 2)', 'add a b = a + b')

  // type
  //eq(1, '\n  \n    1')
  //eq(1, 'ab(1 true).a', 'ab:\n  a int\n  b bool')
  //eq(true, 'ab(1 true).b', 'ab:\n  a int\n  b bool')
  //eq(true, 'ab(1 true) == ab(1 true)', 'ab:\n  a int\n  b bool')
  //eq(false, 'ab(1 true) == ab(2 true)', 'ab:\n  a int\n  b bool')
  //eq(1, 'a(1).x', 'adt|\n  a: x int\n  b: y [int]')
  //eq([1], 'b(1).y', 'adt|\n  a: x int\n  b: y [int]')

//// control flow
//eq(1, 'if(true 1 2)')
//eq(2, 'if(false 1 2)')
//eq(3, 'if(false 1 false 2 3)')
//eq(1, 'if(true 1 not_found)') // check lazy evaluation
//eq(10, 'match(1 1 10 2 20)')
//eq(20, 'match(2 1 10 2 20)')
//eq(99, 'match(3 1 10 2 20 _ 99)')
//eq(99, 'match(3 1 10 2 20 _ 99 _ 999)')
//eq(10, 'match(1 1 10 2 not_found)') // check lazy evaluation
//eq(1, 'f(a(1))', 'f v = match(v a v.x b v.y)', 'adt|\n  a: x int\n  b: y []int')
//eq([1], 'f(b([1]))', 'f v = match(v a v.x b v.y)', 'adt|\n  a: x int\n  b: y []int')
//eq(1, 'f(a)', 'f v = match(v a 1 b 2)', 'adt|\n  a\n  b')
//eq(2, 'f(b)', 'f v = match(v a 1 b 2)', 'adt|\n  a\n  b')
//
//// effect
//eq(3, '\n  count :: 0\n  count += 1\n  count += 2\n  count')
//eq(2, 'f', 'f =\n  n :: 0\n  g =\n    n += 1\n    n\n  g\n  g')
//fail('string.int: not a number hi', '"hi".int')
//eq(0, '"a".int.alt(0)')
//eq(1, '"1".int.alt(0)')
//eq(3, '"1".int.then((x) => x + 2)')
//eq(0, '"a".int.then((x) => x + 2).alt(0)')
//
//// embedded effect
//eq(1, '\n  guard(true)\n  1')
//fail('guard', '\n  guard(false)\n  1')
//eq(2, 'f.then(a => a+1)', 'f =\n  guard(true)\n  1')
//fail('guard', 'f.then(a => a+1)', 'f =\n  guard(false)\n  1')
//eq(2, 'f.alt(2)', 'f =\n  guard(false)\n  1')

  // spiteful tests
  eq(1, ' 1 ')
  eq(1, ' ( ( ( 1 ) ) ) ')
  print('ok')
}
function runStdin() {
  const moa = require('fs').readFileSync('/dev/stdin', 'utf8')
  const result = evaluate(moa)
  if (result.error) {
    warn(result.stderr)
    warn(result.error)
    warn('-- js: ')
    warn(result.js)
    process.exit(1)
  }
  if (result.stderr) {
    warn(result.stderr)
  }
  print(result.stdout)
}
function main() {
  process.argv[2] === 'test' ? runTest() : runStdin()
}
main()
