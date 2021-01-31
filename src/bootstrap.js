'use strict'
//const moa = require('fs').readFileSync('moa.moa', 'utf8')

// utils
const str = obj => JSON.stringify(obj, null, 2)
const put = s => process.stdout.write(s)
const puts = (...a) => console.log(...a)
const dump = o => console.dir(o, {depth: null})
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

// runtime helper functions
function Failure(message) { this.message = message }
Failure.prototype.__failed = true
Failure.prototype.then = function() { return this }
Failure.prototype.alt = v => v
function extend(obj, d) {
  for (const key of Object.keys(d)) {
    const f = d[key]
    if (f.length <= 1) {
      Object.defineProperty(obj.prototype, key, { get: function() { return f(this) } })
    } else {
      obj.prototype[key] = function(...args) { return f(this, ...args) }
    }
  }
  obj.prototype.then = function(f) { return f(this) }
  obj.prototype.alt = function() { return this }
}
const ooo = new Failure('out of index')
extend(Number, {
  string: n => n.toString(),
})
extend(String, {
  char: (s,n) => s[n] || ooo,
  count: s => s.length,
  int: s => s.match(/^[0-9]+$/) ? parseInt(s) : new Failure('string.int: not a number ' + s),
})
extend(Array, {
  at: (a, n) => n < a.length && 0 <= n ? a[n] : ooo,
})
Function.prototype.then = function(f) { return () => this().then(f) }
Function.prototype.alt = function(v) { return () => this().alt(v) }
global.__failure = message => new Failure(message)
global.__eff = o => typeof o === 'function' ? o() : o
global.__equals = (a,b) => a === b || str(a) === str(b)

function evaluate(src, option={}) {
  function tokenize() {
    const consume = (tag,m) => m ? ({tag, code: typeof(m) === 'string' ? m : m[0]}) : null
    const reg = (p,tag,r) => consume(tag, src.slice(p).match(r))
    const some = (p,tag,s) => consume(tag, s.split(' ').find(w => src.slice(p).startsWith(w)))
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
      some(p, 'op2', '+= -= *= /= || && == != >= <= ++ => := <- > < + - * /')

    const liner = (function() {
      const lines = src.split('\n')
      let line = 1
      let column = 1
      return {
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
          const [aname, ...fields] = token.code.split('\n').map(x => x.trim()).filter(x => x)
          token.name = token.type = aname.replace('|', '')
          token.adt = fields.map(field => {
            const [tag, ...kvs] = field.split(/ *[ ,:] */)
            if (kvs.length % 2 !== 0) { throw new Error('The number of ADT fields shoul be even: ' + str(kvs)) }
            return {tag, keys: twin((a,_)=>a, kvs)}
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
      throw new Error('Lack of information after parsing: ' + str({expect,actual,nodes}))
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
    const genLine = t => t.tag === 'id' ? `__e = __eff(${gen(t)}); if (__e.__failed) { return __e }` : gen(t)
    const genLines = ({lines}) => 'function() {\n' + (l => l.slice(0,-1).concat('return ' + l.slice(-1)[0]).join('\n  '))(lines.map(genLine)) + '\n}'
    const genMatch = (cond,body) =>
          cond === '_' ? `true ? ${body} : ` :
          cond.match(/^[a-z_]+$/) ?  `___m.__type === '${cond}' ? ${body} :` :
          `__equals(___m, ${cond}) ? ${body} :`
    function genId(token) {
      if (token.name === 'true' || token.name === 'false' || token.name === '_') {
        return token.name
      } else if (token.name === 'guard') {
        return token.argv.map(gen).join(' && ') + ' ? ({}) : __failure("guard")'
      } else if (token.name === 'if') {
        const l = token.argv.length
        if (l%2!=1) { throw new Error('if arguments have to odd number of arguments: ' + str(token)) }
        const argv = token.argv.map(gen)
        return twin((a,b) => `${a} ? ${b} : `, argv).join('') + argv.slice(-1)[0]
      } else if (token.name === 'match') {
        if (token.argv.length % 2 !== 1) { throw new Error('match arguments have to odd number of arguments: ' + str(token)) }
        const argv = token.argv.map(gen)
        const exps = twin(genMatch, argv, 2)
        return `(___m => ${exps.join(' ')} __failure("miss match"))(${argv[0]})`
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
            case ':=': return 'let ' + gen(token.lhs) + ' = ' + gen(token.rhs)
            case '<-': const name = gen(token.lhs); return 'const ' + name + ' = ' + gen(token.rhs) + '(); if(' + name + '.__failed) { return ' + name + ' }'
            case '=>': return '((' + token.args + ') => ' + gen(token.rhs) + ')'
            case '==': return `__equals(${gen(token.lhs)}, ${gen(token.rhs)})`
            default: return gen(token.lhs) + token.op + gen(token.rhs)
          }
        default: throw new Error('gen ' + str(token))
      }
    }
    return defs.map(gen).join("\n")
  }

  const ret = {}
  const stdout = []
  try {
    const tokens = tokenize()
    ret.defs = parse(tokens)
    ret.js = generate(ret.defs) + '\nreturn typeof(main) === "function" ? main() : main'
    global.print = s => stdout.push(s)
    ret.value = Function(ret.js)()
  } catch (e) {
    ret.error = e
  } finally {
    ret.stdout = stdout.join('\n')
  }
  return ret
}
function runTest() {
  function equals(unwrap, expect, main, ...funcs) {
    const src = funcs.map(x => x + '\n').join('') + 'main = ' + main
    const result = evaluate(src)
    const actual = unwrap(result)
    if (str(expect) === str(actual)) {
      put('.')
    } else {
      console.error('Failed')
      puts('expect: ', expect)
      puts('actual: ', actual)
      puts('src   : ', src)
      put('dump  : ')
      dump(result)
      process.exit(1)
    }
  }
  const eq = (...args) => equals(r => r.value, ...args)
  const stdout = (...args) => equals(r => r.stdout, ...args)
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
  eq(3, '\n  count := 0\n  count += 1\n  count += 2\n  count')
  eq(2, 'f', 'f =\n  n := 0\n  g =\n    n += 1\n    n\n  g\n  g')
  fail('string.int: not a number hi', '"hi".int')
  eq(0, '"a".int.alt(0)')
  eq(1, '"1".int.alt(0)')
  eq(3, '"1".int.then((x) => x + 2)')
  eq(0, '"a".int.then((x) => x + 2).alt(0)')

  // embedded
  stdout('1', 'print(1)')
  stdout('hi', 'print("hi")')

  // embedded string
  eq('1', '1.string')
  eq(2, '"hi".count')
  eq('i', '"hi".char(1)')
  fail('out of index', '"hi".char(2)')

  // embedded array
  eq(1, '[1].at(0)')
  fail('out of index', '[1].at(1)')
  fail('out of index', '[1].at(0-1)')
  eq([2, 3, 4], '[1 2 3].map(x => x + 1)')

  // embedded effect
  eq(1, '\n  guard(true)\n  1')
  fail('guard', '\n  guard(false)\n  1')
  eq(2, 'f.then(a => a+1)', 'f =\n  guard(true)\n  1')
  fail('guard', 'f.then(a => a+1)', 'f =\n  guard(false)\n  1')
  eq(2, 'f.alt(2)', 'f =\n  guard(false)\n  1')

  // spiteful tests
  eq(1, ' 1 ')
  eq(1, ' ( ( ( 1 ) ) ) ')

  console.log('ok')
}
function compileStdin() {
  const moa = require('fs').readFileSync('/dev/stdin', 'utf8')
  const result = evaluate(moa).stdout
  put(result.stdout)
}
function main() {
  process.argv[2] === 'test' ? runTest() : runStdin()
}
main()