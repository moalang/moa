'use strict'

const fs = require('fs')
const str = o => typeof o === 'string' ? o : JSON.stringify(o)
const strs = o => Array.isArray(o) ? o.map(str).join(' ') : str(o)
const put = (...a) => process.stdout.write(strs(a))
const puts = (...a) => console.log(strs(a))
const trace = (...a) => puts('TRACE', ...a)
const dump = o => console.dir(o, {depth: null})
const reserves = 'use fn struct enum let var if else elif for continue break return'.split(' ')

const isOp2 = s => typeof s === 'string' && s.match(/^[+\-*%/=><|&]+$/)
const isEol = s => s === undefined || (typeof s === 'string' && s.match(/[\n{}]/))
const isSpace = s => typeof s === 'string' && s.match(/^[ \t]+$/)
const compileToJs = source => {
  const tokens = source.split(/([A-Za-z0-9_]+\(|[^\[\](){} \n]+|.)/g).filter(x => x.replace(/ +/g, ''))
  const nodes = parse(tokens)
  return generate(nodes)
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
  const consume = t => reserves.includes(t) ? line() : exp()
  const line = () => many(exp, {stop: isEol})
  const exp = () => {
    const lhs = atom()
    if (isOp2(tokens[pos])) {
      return [tokens[pos++], lhs, exp()]
    } else {
      return lhs
    }
  }
  const atom = () => {
    while (isSpace(tokens[pos])) {
      pos++
    }
    if (pos >= tokens.length) {
      return null
    }
    const t = tokens[pos++]
    if (t.match(/^[0-9](\.[0-9]+)?$/) || t.match(/^[A-Za-z0-9_]+$/) || t.startsWith('"')) {
      return t
    } else if (t.match(/^[A-Za-z0-9_]+\($/)) {
      return [t.slice(0, -1),  ...many(exp, {stop: u => u === ')'})]
    } else if ('}])'.includes(t)) {
      return t
    } else if (t === '(') {
      return many(exp, {stop: u => u === ')'})
    } else if (t === '[') {
      return ['__array', ...many(exp, {stop: u => u === ']'})]
    } else if (t === '{') {
      return ['__do', ...many(line, {stop: u => u === '}'})]
    } else {
      throw Error(`Unexpected token "${t}"`)
    }
  }
  return many(consume)
}
const generate = nodes => {
  const gen = o => Array.isArray(o) ?(o.length === 1 ? gen(o[0]) : apply(o)) : o
  const addReturn = a => [...a.slice(-0, -1), 'return ' + a[a.length - 1]]
  const apply = ([head, ...args]) => {
    if (isOp2(head)) {
      return '(' + gen(args[0]) + head + gen(args[1]) + ')'
    } else if (head === '__array') {
      return '[' + args.map(gen).join(', ') + ']'
    } else if (head === '__do') {
      return '(() => {' + addReturn(args.map(gen)).join('\n') + '})()'
    } else if (head === 'let') {
      return `let ${args[0]} = ${gen(args.slice(1))}`
    } else if (head === 'fn') {
      return `const ${args[0]} = (${args.slice(1, -1)}) => ${gen(args[args.length - 1])}`
    } else {
      return `${head}(${args.map(gen)})`
    }
  }
  return nodes.map(gen).join('\n')
}
const test = () => {
  const exp = (expect, source) => {
    const js = compileToJs(source)
    const actual = eval(js)
    if (str(actual) === str(expect)) {
      put('.')
    } else {
      puts('FAILURE')
      puts('source:', source)
      puts('js    :', js)
      puts('expect:', expect)
      puts('actual:', actual)
      process.exit(1)
    }
  }

  // bottom
  exp(1, '1')
  exp('hi', '"hi"')
  exp(1, '(1)')
  exp(1, '((1))')
  exp([1], '[1]')
  exp([1, 2], '[1 2]')
  exp(1, '{1}')
  exp(1, '{let a 1\na}')
  exp(1, 'fn f a a\nf(1)')
  exp(3, 'fn f a b a + b\nf(1 2)')

  // exp
  exp(3, '1 + 2')
  exp(7, '1 + 2 * 3')
  exp(9, '(1 + 2) * 3')

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
| [A-Za-z_][A-Za-z0-9_]* ("(" exp+ ")")?
*/
}
const compile = () => console.log(compileToJs(process.argv[2]))
const main = () => process.argv[2] ? compile(process.argv[2]) : test()

main()
