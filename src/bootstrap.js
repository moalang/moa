'use strict'
//const moa = require('fs').readFileSync('moa.moa', 'utf8')

// utils
const str = obj => JSON.stringify(obj, null, 2)
const put = s => process.stdout.write(s)
const puts = (...a) => console.log(...a)
const dump = o => console.dir(o, {depth: null})
const copy = o => JSON.parse(JSON.stringify(o))
const dig = (d,...args) => args.reduce((o,name) => o[name], d)

// runtime for evaluate
global.__equals = (a,b) => a === b || str(a) === str(b)
global.__match = (a,b) => __equals(a, b)

function evaluate(src) {
  // top: define+
  // define: id+ '=' body
  // body:
  // | ('\n' line)+
  // | exp
  // line:
  // | define
  // | id eff body
  // | exp
  // exp:
  // | '(' exp ')'
  // | unit (op2 exp)*
  // unit:
  // | id '(' exp+ ')'
  // | val
  // val:
  // | [0-9]+
  // | '"' [^"]* '"'
  // id: [a-z][a-z0-9]*
  // op2: + - * || && > >= < <= == !=
  // eff: <- :=
  function tokenize() {
    const lines = src.split('\n')
    function Token(tag, code) {
      this.tag = tag
      this.code = code
    }
    function Liner() {
      this.line = 1
      this.column = 1
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
      reg(p, 'func', /^[a-z_][a-z0-9_]*( +[a-z_][a-z0-9_]*)* +=(?![>=])/) ||
      reg(p, 'struct', /^[A-Za-z_][A-Za-z0-9_]*:(\n  [a-z].*)+/) ||
      reg(p, 'enums', /^[A-Za-z_][A-Za-z0-9_]*\|(\n  [a-z].*)+/) ||
      reg(p, 'int', /^[0-9]+/) ||
      reg(p, 'id', /^[a-z_][a-z0-9_]*/) ||
      reg(p, 'string', /^"(?:(?:\\")|[^"])*"/) ||
      reg(p, 'prop', /^\.[a-z_][a-z0-9_]*\(?/) ||
      reg(p, 'spaces', /^[ \n]+/) ||
      reg(p, 'comment', /^ *#.*/) ||
      some(p, 'la', '[') ||
      some(p, 'ra', ']') ||
      some(p, 'lp', '(') ||
      some(p, 'rp', ')') ||
      some(p, 'op2', '+= -= *= /= || && == != >= <= ++ => := <- > < + - * /')

    const liner = new Liner()
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
      if (token.code === '(' && prev.tag === 'id') {
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
    const eot = {tag: 'EOT', code: ''}
    let pos = 0
    function until(f) {
      const ary = []
      let t
      while (pos < tokens.length && f(t = parseTop())) {
        ary.push(t)
      }
      return ary
    }
    function parseAssert(f, g, expect) {
      const r = f()
      if (!g(r)) {
        throw new Error(`Except ${expect} but ${str(r)}`)
      }
      return r
    }
    function parseCall(token) {
      token.name = escape(token.code.replace('(', '')).replace('.', '')
      token.argv = token.calling ? parseAssert(parseUnit, t => t.code === '(', '(').items : []
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
          [token.name, ...token.args] = token.code.replace('=', '').split(/ +/).slice(0, -1)
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
      throw new Error('Lack of information after parsing: ' + str({expect,actual,tokens}))
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
      '[]': {at: '__arrayAt'},
    }
    const embeddedIds = {
      guard: '__guard',
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
      if (token.args.length > 0) {
        return '(' + token.args.join(',') + ') => ' + body
      } else {
        return body
      }
    }
    function genId(token) {
      if (token.name === 'if') {
        const l = token.argv.length
        if (l%2!=1) { throw new Error('if arguments have to odd number of arguments: ' + str(token)) }
        const argv = token.argv.map(gen)
        const exps = []
        for (let i=1; i<l; i+=2) {
          exps.push(argv[i-1], '?', argv[i], ':')
        }
        exps.push(argv[argv.length-1])
        return exps.join(' ')
      } else if (token.name === 'match') {
        const l = token.argv.length
        if (l%2!=1) { throw new Error('match arguments have to odd number of arguments: ' + str(token)) }
        const argv = token.argv.map(gen)
        const exps = []
        for (let i=2; i<l; i+=2) {
          const cond = argv[i-1]
          if (cond === '_' || i+1 === l) {
            exps.push(argv[i])
            return '(__target => ' + exps.join(' ') + ')(' + argv[0] + ')'
          } else {
            exps.push('__match(__target, ' + argv[i-1] + ')', '?', argv[i] , ':')
          }
        }
        throw new Error('Gennerate Error for match')
      } else if (token.name === 'typeof') {
        return "'" + token.argv[0].type + "'/* typeof " + gen(token.argv[0]) + ' */'
      } else {
        return (embeddedIds[token.name] || token.name) + genCall(token.argv)
      }
    }
    function genProp(token) {
      if (!token.target.type) {
        throw new Error('genProp error: type is undefined ' + str(token))
      }
      const type = token.target.type.startsWith('[]') ? '[]' : token.target.type
      const prop = dig(embeddedProps, type, token.name)
      if (prop) {
        return wrapIfNum(gen(token.target)) + '.' + prop + genCall(token.argv)
      }
      const func = dig(embeddedFuncs, type, token.name)
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
            case '=>': return '((' + token.args.join(',') + ') => ' + gen(token.rhs) + ')'
            case '++': return gen(token.lhs) + '.concat(' + gen(token.rhs) + ')'
            case '->': return genArrow(token.lhs, token.rhs)
            default: return gen(token.lhs) + token.op + gen(token.rhs)
          }
        default: throw new Error('gen ' + str(token))
      }
    }
    return defs.map(gen).join("\n")
  }

  const ret = {value: undefined, stdout: '', stderr: '', error: undefined, js: '', defs: []}
  try {
    const tokens = tokenize()
    ret.defs = parse(tokens)
    ret.js = generate(ret.defs) + '\nreturn typeof(main) === "function" ? main() : main'
    ret.value = Function(ret.js)()
  } catch (e) {
    ret.error = e
  }
  return ret
}
function runTest() {
  function eq(...args) {
    return equals(r => r.value, ...args)
  }
  function stdout(...args) {
    return equals(r => r.stdout, ...args)
  }
  function fail(...args) {
    return equals(r => r.error, ...args)
  }
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
      dump(result)
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

  // function
  eq(3, 'add(1 2)', 'add a b = a + b')

  // type
  eq({a: 1, b: true}, 'ab(1 true)', 'ab:\n  a int\n  b bool')
  eq({ __val: 1, __type: 'a'}, 'a(1)', 'ab|\n  a int\n  b []int')
  eq({ __val: [1], __type: 'b'}, 'b([1])', 'ab|\n  a int\n  b []int')

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

//  // control flow
//  eq(1, 'match(a -> b\n  _ -> c)', 'a = true', 'b = 1', 'c = 2')
//  eq(2, 'match(a -> b\n  _ -> c)', 'a = false', 'b = 1', 'c = 2')
//  eq(2, 'match(a -> b\n  c -> d\n  _ -> e)', 'a = false', 'b = 1', 'c = true', 'd = 2', 'e = 3')
//
//  // pattern match for enum
//  eq(3, 'f(a(1)) + f(a(2))', 'ab|\n  a int\n  b bool', 'f v = match(v:a->v v:b->v)')
//  eq(true, 'f(b(true))', 'ab|\n  a int\n  b bool', 'f v = match(v:a->v v:b->v)')
//
//  // effect
//  eq(3, '\n  count := 0\n  count += 1\n  count += 2\n  count')
//  eq(2, 'f', 'f =\n  n := 0\n  g =\n    n += 1\n    n\n  g\n  g')
//  fail('string.int: not a number hi', '"hi".int')
//  eq(0, '"a".int.alt(0)')
//  eq(1, '"1".int.alt(0)')
//  eq(3, '"1".int.then((x) => x + 2)')
//  eq(0, '"a".int.then((x) => x + 2).alt(0)')
//
//
//  // embedded
//  stdout('1', 'trace(1)')
//  stdout('hi', 'trace("hi")')
//
//  // embedded string
//  eq('1', '1.string')
//  eq('i', '"hi".char(1)')
//  fail('out of index', '"hi".char(2)')
//
//  // embedded array
//  eq(1, '[1]int.at(0)')
//  fail('out of index', '[1]int.at(1)')
//  fail('out of index', '[1]int.at(0-1)')
//  eq([2, 3, 4], '[1 2 3]int.map(x => x + 1)')
//
//  // embedded effect
//  eq(1, '\n  guard(true)\n  1')
//  fail('guard', '\n  guard(false)\n  1')
//
//  // embedded typeof
//  eq('int', 'typeof(1)')
//  eq('string', 'typeof("hi")')
//  eq('int', 'typeof(f)', 'f = 1')
//  eq('func', 'typeof(f)', 'f a = a')
//  eq('int', 'typeof(g)', 'f = 1', 'g = f')
//  eq('func', 'typeof(g)', 'f a = 1', 'g = f')
//  eq('int', 'typeof(match(true -> 1))')
//  eq('int', 'typeof(match(f:a -> f))', 'ab|\n  a  int\n  b  bool', 'f = a(1)')
//  eq('bool', 'typeof(match(f:a -> f))', 'ab|\n  a  int\n  b  bool', 'f = b(true)')
//  eq('string', 'typeof(match(f:a -> "a"))', 'ab|\n  a  int\n  b  bool', 'f = b(true)')
//  eq('eff', 'typeof(f)', 'f = trace(1)')
//  eq('eff', 'typeof(f)', 'f =\n  n:=0\n  n')
//  eq('int', 'f', 'f =\n  n:=0\n  typeof(n)')

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
