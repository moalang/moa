'use strict'

Error.stackTraceLimit = 100

const fs = require('fs')
const str = o => typeof o === 'string' ? o : JSON.stringify(o)
const strs = o => Array.isArray(o) ? o.map(str).join(' ') : str(o)
const put = (...a) => process.stdout.write(strs(a))
const puts = (...a) => console.log(strs(a))
const dump = (...a) => a.map(o => console.dir(o, {depth: null}))
const trace = o => { dump(o); return o }
const reserves = 'let var fn struct when if unless for while continue break return fail p pp)'.split(' ')
const __prop = (obj, id, args) => {
  const t = typeof obj
  if ((t === 'string' || Array.isArray(obj)) && id === 'size') {
    return obj.length
  }
}

function Token(code, pos) { this.code = code; this.pos = pos }
Token.prototype.toString = function() { return this.code }
function Group(tag, a) { this[tag] = true; this.a = a }
Group.prototype.toString = function() { return '(' + this.a.map(x => x.toString()).join(' ') + ')' }

const newToken = (code, pos) => new Token(code, pos)
const newArray = a => new Group('array', a)
const newStruct = a => new Group('struct', a.map(x => x.code ? ['=', x, x] : x))
const newStatement = a => new Group('statement', a)

const isOp2 = t => t && t.toString().match(/^[+\-*%/=><|&^]+$/)
const compile = source => {
  const tokens = tokenize(source)
  const nodes = parse(tokens)
  const js = generate(nodes)
  return {tokens, nodes, js}
}

const tokenize = source => {
  const simplify = ts => {
    let pos = 0
    const safe = t => t === '{' ? '{{' : t === '}' ? '}}' : t
    ts = ts.map(code => { const t = newToken(safe(code), pos); pos += code.length; return t })
    ts = ts.filter(x => x.code.replace(/^ +/, ''))

    let nesting = 0
    let indent = 0
    const close = n => [...Array(n)].flatMap(_ => [';', '}', ';']).map(t => newToken(t, -1))
    const convert = t => {
      if (t == ':') {
        return newToken('{', t.pos)
      } else if (nesting === 0 && t.code.includes('\n')) {
        const before = indent
        indent = t.code.split('\n').slice(-1)[0].length
        if (indent % 2 !== 0) {
          throw Error(`Indentations must be multiple of two spaces. But this is ${JSON.stringify(indent)}`)
        }
        if (indent == before) {
          return newToken(';', t.pos)
        } else if (indent < before) {
          return close((before - indent) / 2)
        }
        return []
      } else if ('[('.includes(t.code)) {
        ++nesting
      } else if (')]'.includes(t.code)) {
        --nesting
      }
      return t
    }
    return ts.flatMap(convert).concat(close(indent / 2))
  }
  return simplify(source.split(/([ \n]+|[0-9]+(?:\.[0-9]+)?|[A-Za-z0-9_]+(?:,[A-Za-z0-9_]+)*|[+\-*%/=><|&^]+|"[^"]*"|`[^`]*`|[^\[\](){} \n;\.]+|.)/g))
}

const parse = tokens => {
  let pos = 0
  const many = (f, option, g) => {
    option = option || {}
    const a = []
    while (pos < tokens.length) {
      if (option.stop && option.stop(tokens[pos])) {
        ++pos
        break
      }
      a.push(f(tokens[pos]))
    }
    return g ? g(a) : a
  }
  const consume = t => reserves.includes(t.code) ? line() : exp()
  const line = () => many(exp, {stop: t => t == ';'}, a => a.length === 1 ? a[0] : a)
  const exp = () => {
    const lhs = atom()
    const t = tokens[pos]
    if (t && isOp2(t)) {
      ++pos
      return [t, lhs, exp()]
    } else {
      return lhs
    }
  }
  const atom = () => {
    const unit = bottom()
    const pred = tokens[pos]
    if (pred == '.') {
      ++pos
      return [tokens[pos-1], unit, atom()]
    } else if (pred == '(' && tokens[pos - 1].pos === pred.pos - tokens[pos - 1].code.length) {
      return [unit].concat(bottom())
    } else {
      return unit
    }
  }
  const bottom = () => {
    if (pos >= tokens.length) {
      return null
    }
    const token = tokens[pos++]
    const code = token.code
    if (code.match(/^[A-Za-z0-9_]+/) || code.startsWith('"') || code.startsWith('`')) {
      return token
    } else if ('}]);'.includes(code)) {
      return token
    } else if (token == '{{') {
      return many(exp, {stop: u => u == '}}'}, newStruct)
    } else if (token == '(') {
      return many(exp, {stop: u => u == ')'}, a => a.length === 1 ? a[0] : a)
    } else if (token == '[') {
      return many(exp, {stop: u => u == ']'}, newArray)
    } else if (token == '{') {
      return many(line, {stop: u => u == '}'}, newStatement)
    } else if (isOp2(code)) {
      return token
    } else {
      throw Error(`Unexpected token "${code}"`)
    }
  }
  return many(consume)
}

const generate = nodes => {
  const gen = o => o.array ? '[' + o.a.map(gen) + ']' :
    o.struct ? '({' + o.a.map(x => `${x[1].code}: ${gen(x[2])}`) + '})' :
    o.statement ? statement(o.a.map(gen)) :
    Array.isArray(o) ? apply(o) : o.code
  const isCond = args => args.length >= 2 && args[args.length - 2].code === 'if'
  const cond = (head, args) => args.length === 0 ? head.code :
    args[0] == 'if' ? `if (${gen(args[1])}) ${head}` :
    args[0] == 'unless' ? `if (!${gen(args[1])}) ${head}` :
    fail(`Unknown condition ${args}`)
  const addReturn = x => x.match(/^return|if|for|while/) ? x : 'return ' + x
  const statement = a => `(() => { ${[...a.slice(0, -1), addReturn(a[a.length - 1])].join(';')} })()`
  const block = a => a[0].statement ? '{' + a.slice(1).map(gen).join(';') + '}' : gen(a)
  const when = a => a.length === 1 ? a[0] : `${a[0]} ? ${1} : ` + when(a.slice(2))
  const apply = node => {
    const [head, ...args] = node
    if (Array.isArray(head) || head.struct || head.statement) {
      return args.length ? gen(head) + '(' + args.map(gen) + ')' : gen(head)
    } else if (head.array) {
      return args.length ? gen(head) + '[' + args.map(gen) + ']' : gen(head)
    } else if (isOp2(head)) {
      if (head == '=>') {
        return '((' + gen(args[0]) + ') => ' + gen(args[1]) + ')'
      } else {
        return '(' + gen(args[0]) + head + gen(args[1]) + ')'
      }
    } else if (head == '.') {
      if (Array.isArray(args[1])) {
        return `__prop(${gen(args[0])}, '${args[1][0]}', ${args[1].slice(1).map(gen)})`
      } else {
        return `__prop(${gen(args[0])}, '${args[1]}')`
      }
    } else if (head == 'let') {
      return `const ${gen(args[0])} = ${gen(...args.slice(1))}`
    } else if (head == 'var') {
      return `var ${gen(args[0])} = ${args.length >= 2 ? gen(args.slice(1)) : 'undefined'}`
    } else if (head == 'fn') {
      return `const ${gen(args[0])} = (${args.slice(1, -1).map(gen)}) => ${gen(args[args.length - 1])}`
    } else if (head == 'struct') {
      const names = args[args.length - 1].slice(1).map(a => '_' + a[0])
      return `const ${gen(args[0])} = (${names}) => ({${names}})`
    } else if (head == 'if') {
      return `if (${gen(args[0])}) ${block(args[1])} ${args.length >= 3 ? gen(args.slice(2)) : ''}`
    } else if (head == 'elif') {
      return `else if (${gen(args[0])}) ${block(args[1])} ${args.length >= 3 ? gen(args.slice(2)) : ''}`
    } else if (head == 'else') {
      return `else ${block(args)}`
    } else if (head == 'when') {
      return when(args)
    } else if (head == 'return') {
      if (args.length === 0) {
        return 'return'
      } else if (args.length === 1) {
        return `return ${gen(args[0])}`
      } else if (args.length === 2) {
        return cond('return', args)
      } else if (args.length === 3) {
        return cond(`return ${args[0]}`, args.slice(1))
      } else {
        throw Error(`Unknown return syntax ${args}`)
      }
    } else if (head == 'p' || head == 'pp') {
      if (isCond(args)) {
        return cond(`console.log(${args.slice(0, -2).map(gen)})`, args.slice(-2))
      } else {
        return `console.log(${args.map(gen)})`
      }
    } else {
      return `${head}(${args.map(gen)})`
    }
  }
  return nodes.map(gen).join(';\n')
}

const check = (expect, exp, ...defs) => {
  const source = (defs || []).concat(`fn main:\n  ${exp.replace("\n", "\n  ")}`).join('\n')
  const {js, nodes, tokens} = compile(source)
  let actual
  try {
    actual = eval(js + '\nmain()')
  } catch(e) {
    actual = e.stack
  }
  if (str(actual) === str(expect)) {
    put('.')
  } else {
    const unwrapToken = o => Array.isArray(o) ? o.map(unwrapToken) : o.a ? o.a.map(unwrapToken) : o.toString()
    puts('FAILURE')
    puts('source:', source)
    puts('js    :', js)
    puts('nodes :', nodes.map(unwrapToken))
    puts('tokens:', tokens.map(unwrapToken))
    puts('expect:', expect)
    puts('actual:', actual)
    process.exit(3)
  }
}

const testBootstrap = () => {
  // -- Tests for executions
  // node:
  // | keywords exp+ (":" ("\n  " node)+)? cond? "\n"
  // | exp+ cond? "\n"
  // exp: unit (op2 exp)*
  check(3, '1 + 2')
  // unit: bottom ("." id ("(" exp+ ")")?)*
  check(1, '[1].size')
  check(2, '"hi".size')
  // bottom:
  // | "(" exp ")"                     # priority : 1 * (2 + 3)
  check(9, '(1 + 2) * 3')
  // | "[" exp* "]"                    # array    : [] [1 2]
  check([], '[]')
  check([1, 2], '[1 2]')
  check(2, '[1 2](1)')
  // | "{" tags "}"                    # struct   : {} {one two=2 (three)=3}
  check({}, '{}')
  check({key:1}, '{key=1}')
  check({key1:1, key2:2}, '{key1 key2=2}', 'let key1 1')
  // | '"' [^"]* '"'                   # string   : "hi"
  check('hi', '"hi"')
  check('${name}', '"${name}"')
  // | '`' ("${" unit "}" | [^"])* '`' # template : "hi {name}"
  check('hello moa', '`hello ${name}`', 'let name "moa"')
  // | id ("," id)* "=>" exp           # lambda   : a,b => a + b
  check('3', '(x => x + 1)(2)')
  check('3', '(a,b => a + b)(1 2)')
  // | [0-9]+ ("." [0-9]+)?            # number   : 1 0.5
  check(1, '1')
  check(1.5, '1.5')
  // | id ("(" exp+ ")")?              # id       : name f()
  check(1, 'v', 'let v 1')
  check(1, 'f()', 'fn f 1')
  check(2, 'inc(1)', 'fn inc x:\n  x + 1')
  check(3, 'add(1 2)', 'fn add a b:\n  a + b')
  // keyword: qw(let var fn struct if unless for while continue break return fail p pp)
  check(1, 'when true 1 2')
  check(2, 'when false 1 2')
  check(3, 'when false 1 false 2 3')

  puts('ok')
}

const tester = () => {
  const moa = fs.readFileSync('./moa.moa', 'utf-8')
  fs.writeFileSync('/tmp/moa', compile(moa).js)
  const { execSync } = require('child_process')
  execSync('node /tmp/moa moa.moa > ../bin/moa')
  execSync('chmod 0755 ../bin/moa')
  return (expect, source) => {
    const actual = execSync('node ../bin/moa /tmp/t', {encoding: 'utf-8'})
    if (expect === actual) {
      put('.')
    } else {
      puts('source:', source)
      puts('js    :', fs.readFileSync('../bin/moa', 'utf-8'))
      puts('expect:', expect)
      puts('actual:', actual)
    }
  }
}

const testMoa = () => {
  const eq = tester()
  eq('1\n',  'p 1')
  puts('ok')
}

const main = () => {
  const paths = process.argv.slice(2)
  testBootstrap()
  testMoa()
}

main()
