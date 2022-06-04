'\nuse strict'

Error.stackTraceLimit = 100

// general helper
const str = o => typeof o === 'string' ? o : JSON.stringify(o)
const strs = o => Array.isArray(o) ? o.map(str).join(' ') : str(o)
const put = (...a) => process.stdout.write(strs(a))
const puts = (...a) => console.log(strs(a))
const dump = (...a) => a.map(o => console.dir(o, {depth: null}))
const trace = o => { dump(o); return o }

// specific helper
const isOp2 = t => typeof t === 'string' && t.match(/^[+\-*%/=><|&^]+$/)
const isAssign = t => t.endsWith('=')
const isOp1 = t => t === '!'

// runtime
const __prop = (obj, id, args) => {
  if ((typeof obj === 'string' || Array.isArray(obj)) && id === 'size') {
    return obj.length
  }
  const o = obj[id]
  return args.length ? o(...args) : o
}
const __at = (a, n) => a[n]
let __p = (...a) => console.log(...a)

// compiler
const compile = source => {
  const tokens = tokenize(source)
  const nodes = parse(tokens)
  const js = generate(nodes)
  return {tokens, nodes, js}
}

const tokenize = source => {
  const simplify = tokens => {
    let nesting = 0
    let indent = 0
    const close = n => [...Array(n)].flatMap(_ => ['__eol', '__eob', '__eol'])
    const convert = (token, i) => {
      if (!token || token.startsWith('#') || !token.replace(/^ +$/, '')) {
        return []
      }

      if (nesting === 0 && token.match(/^[ \n]+/)) {
        const before = indent
        indent = token.split('\n').slice(-1)[0].length
        if (indent % 2 !== 0) {
          throw Error(`Indent error: '${JSON.stringify(indent)}'`)
        }
        if (indent === before) {
          return '__eol'
        } else if (indent < before) {
          return close((before - indent) / 2)
        }
        return []
      } else if ('{[('.includes(token)) {
        ++nesting
      } else if (')]}'.includes(token)) {
        --nesting
      }

      const prev = tokens[i - 1] || ''
      const pred = tokens[i + 1] || ''
      if (token === '(' && prev && prev.match(/^[A-Za-z0-9_)\]]/)) {
        return '__call'
      }
      if (token === '[' && prev && prev.match(/^[A-Za-z0-9_)\]]/)) {
        return '__at'
      }
      if (token === ':' && pred && !pred.includes('\n')) {
        return '__line'
      }
      return token
    }
    return tokens.flatMap(convert).concat(close(indent / 2))
  }
  return simplify(source.split(/((?: |\n|#[^\n]*)+|[0-9]+(?:\.[0-9]+)?|[A-Za-z0-9_]+(?:,[A-Za-z0-9_]+)*|[+\-*%/=><|&^]+|"[^"]*"|`[^`]*`|.)/g).filter(x => x))
}

const parse = tokens => {
  let pos = 0
  const many = (f, stop, g) => {
    const a = []
    while (pos < tokens.length) {
      const token = tokens[pos]
      if (stop == token) {
        ++pos
        break
      } else if (token === '__line') {
        ++pos
        a.push(many(exp))
      } else if (token.startsWith('__')) {
        break
      } else {
        a.push(f(token))
      }
    }
    return g ? g(a) : a
  }
  const line = () => many(exp, '__eol')
  const exp = () => glue(bottom())
  const glue = lhs => {
    const pred = tokens[pos++]
    if (isOp1(lhs)) {
      --pos
      return [lhs, exp()]
    } else if (isOp2(pred)) {
      return [pred, lhs, exp()]
    } else if (pred == '.') {
      return [tokens[pos-1], lhs, exp()]
    } else if (pred == '__call') {
      return glue(many(exp, ')', a => ['__call', lhs, ...a]))
    } else if (pred === '__at') {
      return glue([pred, lhs, many(exp, ']')])
    } else {
      --pos
      return lhs
    }
  }
  const bottom = () => {
    if (pos >= tokens.length) {
      return null
    }
    const token = tokens[pos++]
    if (token.match(/^[A-Za-z0-9_"`}\])]/) || isOp1(token)) {
      return token
    } else if (token === '{') {
      return many(exp, '}', a => ['__struct', ...a])
    } else if (token === '(') {
      return many(exp, ')')
    } else if (token === '[') {
      return many(exp, ']', a => ['__array', ...a])
    } else if (token === ':') {
      return many(line, '__eob', a => ['__block', ...a])
    } else {
      throw Error(`Unexpected token "${token}"`)
    }
  }
  const unnest = a => !Array.isArray(a) ? a : a.length === 1 ? unnest(a[0]) : a.map(unnest)
  return many(line).map(unnest)
}

const generate = nodes => {
  const gen = o => Array.isArray(o) ? apply(o) :
    o === '__array' ? '[]' :
    o === '__struct' ? '({})' :
    o.startsWith('`') ? template(o) : o
  const template = s => s.replace(/\$[^ `]+/g, x => '${' + x.slice(1) + '}')
  const when = a => a.length === 1 ? a[0] : `${a[0]} ? ${a[1]} : ` + when(a.slice(2))
  const statement = a => a.map((v, i) => a.length - 1 === i ? 'return ' + v : v)
  const apply = node => {
    const [head, ...tail] = node
    const args = tail.map(gen)
    // internal marks
    if (head === '__call') {
      return args[0] + `(${args.slice(1)})`
    } else if (head === '__block') {
      return args.length === 1 ? args[0] : '{\n  ' + statement(args).join('\n  ') + '\n}'
    } else if (head === '__array') {
      return `[${args.join(', ')}]`
    } else if (head === '__struct') {
      const kvs = tail.map(o => Array.isArray(o) ? `${o[1]}:${o[2]}` : o).join(',')
      return `({${kvs}})`

    // operators
    } else if (head === '.') {
      args[1] = `'${args[1]}'`
      return `__prop(${args})`
    } else if (isOp1(head)) {
      return '(' + head + args.join('') + ')'
    } else if (isOp2(head)) {
      const [lhs, rhs] = args
      if (head === '=>') {
        return `((${lhs}) => ${rhs})`
      } else if (isAssign(head)) {
        return `${lhs} ${head} ${rhs}`
      } else {
        return `(${lhs} ${head} ${rhs})`
      }

    // keywords
    } else if (head === 'let') {
      return `const ${args[0]} = ${args[1]}`
    } else if (head === 'var') {
      return `let ${args[0]} = ${args[1]}`
    } else if (head === 'fn') {
      return `const ${args[0]} = (${args.slice(1, -1)}) => ${args[args.length - 1]}`
    } else if (head === 'struct') {
      const names = tail[1].slice(1).map(x => x[0])
      return `const ${tail[0]} = (${names}) => ({${names}})`
    } else if (head === 'adt') {
      const names = tail[1].slice(1).map(x => x[0])
      return names.map(name => `const ${name} = __val => ({__tag:'${name}',__val})`).join('\n')
    } else if (head === 'match') {
      const error = '(()=>{throw Error(`Unmatch error: "${__.__tag}"`)})()'
      const match = tail[1].slice(1).map(a => `__.__tag === '${a[0]}' ? (${gen(a[1])} => ${gen(a[2])})(__.__val) : `).join('\n  ') + error
      return `(__ => ${match})(${args[0]})`
    } else if (head === 'when') {
      return when(args)
    } else if (head === 'if') {
      return `${args[0]} ? ${args[1]} : null`
    } else if (head === 'p') {
      return `__p(${args})`

    // general
    } else {
      return `${head}(${args})`
    }
  }
  return nodes.map(gen).join(';\n')
}

// checker
const check = (expect, exp, ...defs) => {
  const source = (defs || []).concat(exp).join('\n')
  try {
    const {js, nodes, tokens} = compile(source)
    let actual
    try {
      const stdout = []
      const __p = (...a) => stdout.push(a.map(str).join(' '))
      actual = eval(js)
      if (stdout.length) {
        actual = stdout.join('\n')
      }
    } catch(e) {
      actual = e.stack
    }
    if (str(actual) === str(expect)) {
      put('.')
    } else {
      puts('FAILURE')
      puts('source:', source)
      puts('js    :', js)
      puts('nodes :', nodes)
      puts('tokens:', tokens)
      puts('expect:', expect)
      puts('actual:', actual)
      process.exit(1)
    }
  } catch (e) {
      puts('FAILURE')
      puts('source:', source)
      puts('error:', e.stack)
      process.exit(1)
  }
}

const test = () => {
  // top: node*
  check(3, 'a + b()', 'let a 1\nfn b 2')
  // node: exp+ ("\n" | (":\n" ("  " node)+)?)
  // exp: unit (op2 exp)*
  check(7, '1 + 2 * 3')
  // unit: op1? bottom (prop | call | at)*
  check(false, '!true')
  check(true, '!(true && false)')
  check(1, 'f()[1].size', 'fn f: ["a" "b"]')
  // prop: "." id
  check(2, '"hi".size')
  // call: "(" exp+ ")"
  check(3, 'add(1 2)', 'fn add a b: a + b')
  check(1, 'f()(1)', 'fn f: g\nfn g x: x')
  // at: "[" exp "]"
  check(2, '[1 2][1]')
  check(2, 'a[1]', 'let a [1 2]')
  check(1, 'a[0][0]', 'let a [[1]]')
  // bottom:
  // | "(" exp  ")"                    # priority : 1 * (2 + 3)
  check(9, '(1 + 2) * 3')
  // | "(" exp exp+ ")"                # apply    : f(a) == (f a)
  check(3, '(add 1 2)', 'fn add a b: a + b')
  // | "[" exp* "]"                    # array    : [], [1 2]
  check([], '[]')
  check([1, 2], '[1 2]')
  // | "{" id* (id "=" exp)* "}"       # struct   : {}, {one two=2}
  check({}, '{}')
  check({one: 1, two: 2}, '{one two=2}', 'let one 1')
  // | '"' [^"]* '"'                   # string   : "hi"
  check('hi', '"hi"')
  check('hi', '"hi"')
  // | '`' ("$" unit | [^"])* '`'      # template : `hi $user.name`
  check('hi moa', '`hi $name`', 'let name "moa"')
  check('a\nb', '`a\nb`')
  // | id ("," id)* "=>" exp           # lambda   : a,b => a + b
  check(1, 'f(a => 1)', 'fn f g: g()')
  check(3, 'f(a,b => 3)', 'fn f g: g(1 2)')
  // | [0-9]+ ("." [0-9]+)?            # number   : 1, 0.5
  check(1, '1')
  check(0.5, '0.5')
  // | id                              # id       : name
  check(1, 'a', 'let a 1')

  // qw(let var fn struct adt if when match)
  check(1, 'n', 'let n 1')
  check(1, 'var n 0\nn+=1\nn')
  check(1, 'f()', 'fn f: 1')
  check({name: 'apple', price: 199}, 'item("apple" 199)', 'struct item:\n  name string\n  price int')
  check(3, 'match a(1):\n  a v: v + 2\n  b v: v', 'adt ab:\n  a int\n  b string')
  check(2, 'match b("hi"):\n  a v: v\n  b v: v.size', 'adt ab:\n  a int\n  b string')
  check(1, 'if true: p 1')
  check(null, 'if false: p 1')
  check(3, 'a + f()', 'let a 1\nfn f: 2')
  check(1, 'when true 1 2')
  check(2, 'when false 1 2')
  check(2, 'when false 1 true 2 3')
  check(3, 'when false 1 false 2 3')

  puts('ok')
}

const selfhceck = () => {
  const fs = require('fs')
  const moa = fs.readFileSync('./moa.moa', 'utf-8')
  const main_go = fs.readFileSync('./main.go', 'utf-8')
  const moa_go = eval(compile(moa).js + `\ncompile(${JSON.stringify(moa)})`)
  const go = main_go + '\n' + moa_go + '\n'
  fs.writeFileSync('/tmp/moa.go', go)
  const { execSync } = require('child_process')
  const run = cmd => put(execSync(cmd, {encoding: 'utf-8'}))
  const rets = []
  run('go build /tmp/moa.go')
  run('mv moa ../bin/moa')
  run('chmod 0755 ../bin/moa')
  run('../bin/moa --selfcheck')
}

test()
selfhceck()
