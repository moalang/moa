'use strict'

const fs = require('fs')
const str = o => typeof o === 'string' ? o : JSON.stringify(o)
const strs = o => Array.isArray(o) ? o.map(str).join(' ') : str(o)
const put = (...a) => process.stdout.write(strs(a))
const puts = (...a) => console.log(strs(a))
const trace = (...a) => puts('TRACE', ...a)
const dump = o => console.dir(o, {depth: null})
const reserves = 'use fn struct enum let var if else elif for continue break return'.split(' ')
const newNode = s => new String(s)
const __method = {
  string: {
    size: o => o.length
  }
}

const isOp2 = s => s && s.match && s.match(/^[+\-*%/=><|&.]+$/)
const compile = source => {
  const simplify = ts => {
    const tokens = []
    let level = 0
    for (const t of ts) {
      if (t === ' ' || t.match(/^ +$/)) {
        continue
      } else if (t.match(/^ *$/)) {
        continue
      } else if (t.includes('\n')) {
        if (level === 0) {
          tokens.push(';')
        }
        continue
      }
      if ('[('.includes(t)) {
        ++level
      } else if (')]'.includes(t)) {
        --level
      } else if (t === '}' && tokens[tokens.length - 1] !== ';') {
        tokens.push(';')
      }
      tokens.push(t)
    }
    return tokens.map(t => newNode(t))
  }
  const tokens = simplify(source.split(/([A-Za-z0-9_]+\(|[^\[\](){} \n;\.]+|.)/g))
  const nodes = inference(parse(tokens))
  const js = generate(nodes)
  return {tokens, nodes, js}
}

const parse = tokens => {
  let pos = 0
  const many = (f, option) => {
    option = option || {}
    const a = []
    while (pos < tokens.length) {
      if (option.stop && option.stop(tokens[pos])) {
        ++pos
        break
      }
      a.push(f(tokens[pos]))
    }
    return a
  }
  const consume = t => reserves.includes(t.toString()) ? line() : exp()
  const line = () => many(exp, {stop: t => t == ';'})
  const exp = () => ((lhs, t) => isOp2(t) ? ++pos && [t, lhs, exp()] : lhs)(atom(), tokens[pos])
  const atom = () => {
    if (pos >= tokens.length) {
      return null
    }
    const t = tokens[pos++]
    if (t.match(/^[0-9](\.[0-9]+)?$/) || t.match(/^[A-Za-z0-9_]+$/) || t.startsWith('"')) {
      return t
    } else if (t.match(/^[A-Za-z0-9_]+\($/)) {
      return [newNode(t.slice(0, -1)),  ...many(exp, {stop: u => u == ')'})]
    } else if ('}]);'.includes(t.toString())) {
      return t
    } else if (t == '(') {
      return many(exp, {stop: u => u == ')'})
    } else if (t == '[') {
      return [newNode('__array'), ...many(exp, {stop: u => u == ']'})]
    } else if (t == '{') {
      return [newNode('__do'), ...many(line, {stop: u => u == '}'})]
    } else {
      throw Error(`Unexpected token "${t}"`)
    }
  }
  return many(consume)
}

const inference = nodes => {
  const inf = o => Array.isArray(o) ? o.map(inf) : o.type = 'string'
  nodes.map(inf)
  return nodes
}

const generate = nodes => {
  const gen = o => Array.isArray(o) ?(o.length === 1 ? gen(o[0]) : apply(o)) : o
  const addReturn = a => [...a.slice(-0, -1), 'return ' + a[a.length - 1]]
  const apply = ([head, ...args]) => {
    if (isOp2(head)) {
      if (head == '.') {
        return `__method.${args[0].type}.${args[1]}(${args[0]})`
      } else {
        return '(' + gen(args[0]) + head + gen(args[1]) + ')'
      }
    } else if (head == '__array') {
      return '[' + args.map(gen).join(', ') + ']'
    } else if (head == '__do') {
      return '(() => {' + addReturn(args.map(gen)).join('\n') + '})()'
    } else if (head == 'let') {
      return `let ${args[0]} = ${gen(args.slice(1))}`
    } else if (head == 'fn') {
      return `const ${args[0]} = (${args.slice(1, -1)}) => ${gen(args[args.length - 1])}`
    } else {
      return `${head}(${args.map(gen)})`
    }
  }
  return nodes.map(gen).join('\n')
}
const test = () => {
  const exp = (expect, source) => {
    const {js, nodes, tokens} = compile(source)
    let actual
    try {
      actual = eval(js)
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
  }

  // bottom
  exp(1, '1')
  exp('hi', '"hi"')
  exp(2, '"hi".size')
  exp(1, '(1)')
  exp([1], '[1]')
  exp([1, 2], '[1 2]')
  exp(1, '{1}')
  exp(2, '{1;2}')
  exp(2, '{1\n2}')
  exp(1, '{let a 1\na}')

  // reverses: use
  // reverses: fn
  exp(1, 'fn f a a\nf(1)')
  exp(3, 'fn f a b a + b\nf(1 2)')
  // reverses: struct
  ////exp(1, 'struct a { b int; c string }\na(1 "hi").b')
  // reverses: enum
  // reverses: let
  // reverses: var
  // reverses: if
  // reverses: else
  // reverses: elif
  // reverses: for
  // reverses: continue
  // reverses: break
  // reverses: return

  // exp
  exp(3, '1 + 2')
  exp(7, '1 + 2 * 3')
  exp(9, '(1 + 2) * 3')

  // new lines
  exp(1, '(\n(\n1\n)\n)')
  exp(9, '(\n1\n+\n2\n) * 3')
  exp([1, 2], '[\n1\n2\n]')

  // reserves

  // lines
  puts('ok')
/*
top: line+
line:
| reserves? exp+ "\n"
| exp "\n"
reserves: qw(use fn struct enum let var if else elif for continue break return)
node: bottom ("." id ("(" exp+ ")"))*
bottom:
| "[" (":" | (id ":" exp)+) "]" # dict?
*/
}
const main = () => process.argv[2] ? console.log(compile(process.argv[2]).js) : test()

main()
