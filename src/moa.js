'use strict'

const fs = require('fs')
const str = o => typeof o === 'string' ? o : JSON.stringify(o)
const strs = o => Array.isArray(o) ? o.map(str).join(' ') : str(o)
const put = (...a) => process.stdout.write(strs(a))
const puts = (...a) => console.log(strs(a))
const dump = o => console.dir(o, {depth: null})
const reserves = 'use fn struct enum let var if else elif for continue break return'.split(' ')

const isOp2 = s => typeof s === 'string' && s.match(/^[+\-*%/=><|&]+$/)
const compileToJs = source => {
  const tokens = source.split(/([^\[\](){} \n]+|.)/g).filter(x => x.trim())
  const nodes = parse(tokens)
  return generate(nodes)
}
const parse = tokens => {
  let pos = 0
  const many = f => {
    const a = []
    while (pos < tokens.length) {
      if (tokens[pos].match(/^ +$/)) {
        pos++
      } else {
        a.push(f())
      }
    }
    return a
  }
  const consume = () => reserves.includes(tokens[pos]) ? line() : exp()
  const drop = (ret, g) => {
    if (!g(tokens[pos])) {
      throw Error(`Unexpected token: ${g.toString()} but ${tokens[pos]}`)
    }
    ++pos
    return ret
  }
  const line = () => drop(many(exp), u => u === undefined || u.includes('\n'))
  const exp = () => {
    const lhs = atom()
    if (isOp2(tokens[pos])) {
      return [tokens[pos++], lhs, exp()]
    } else {
      return lhs
    }
  }
  const atom = () => {
    const t = tokens[pos++]
    if (t === '(') {
      return drop(exp(), u => u === ')')
    } else if (t === '[') {
      return drop(many(exp), u => u === ']')
    } else if (t === '{') {
      return drop(many(line), u => u === '}')
    } else if (t === '"') {
      return t
    } else if (t.match(/^[0-9](\.[0-9]+)?$/)) {
      return t
    } else if (t.match(/^[A-Za-z0-9_]+$/)) {
      return t
    } else {
      throw Error(`Unknown token ${t}`)
    }
  }
  return many(consume)
}
const generate = nodes => {
  const gen = o => Array.isArray(o) ? apply(o) : bottom(o)
  const bottom = o => o
  const apply = ([head, ...args]) => {
    if (isOp2(head)) {
      return gen(args[0]) + head + gen(args[1])
    } else {
      throw Error(`Unknown node ${head} ${args}`)
    }
  }
  return nodes.map(gen).join('\n')
}
const test = () => {
  const exp = (expect, source) => {
    const actual = eval(compileToJs(source))
    if (str(actual) === str(expect)) {
      put('.')
    } else {
      puts('source:', source)
      puts('expect:', expect)
      puts('actual:', actual)
    }
  }
  exp(3, '1 + 2')
  puts('ok')
}
const compile = () => console.log(compileToJs(process.argv[2]))
const main = () => process.argv[2] ? compile(process.argv[2]) : test()

main()
