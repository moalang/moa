'use strict'
const write = (...a) => process.stdout.write(a.map(t => t.toString()).join(' '))
const puts = (...a) => console.log(...a)
const dump = o => console.dir(o, {depth: null})
const trace = (...a) => (puts(...a), a[a.length - 1])
const str = o => JSON.stringify(o)
const eq = (l, r) => str(l) === str(r)
const fail = (label, o) => { throw new Error(label + ' ' + str(o)) }
const isArray = o => typeof o === 'object' && o.constructor === Array

const run = src => {
  const tokens = tokenize(src)
  const nodes = parse(tokens)
  const js = generate(nodes) + '\nreturn main()'
  return Function(js)()
}
const tokenize = src => src.match(/[()[]:=]|[+\-\*\/!=<>|&]+|\n *| +|[0-9]+(?:\.[0-9]+)?|\.?[a-zA-Z_]+[a-zA-Z_0-9]*\(?/g).filter(t => !t.match(/^ *$/))
const parse = tokens => {
  let pos = 0
  const len = tokens.length
  const isOp2 = t => '+-*/=!|&>'.includes(t[0])
  const check = f => pos < len && f(tokens[pos])
  const many = (acc, f) => check(t => (v => many(acc.concat([v]), f))(f(t))) || acc

  const value = () => {
    const t = tokens[pos++]
    switch (t) {
      case '(': return many([], t => t === ')' ? !++pos : exp())
      case '[': return many(['array'], t => t === ']' ? !++pos : exp())
      case '{': return many(['struct'], t => t === '}' ? !++pos : exp())
      default: return t
    }
  }
  const block = () => check(indent => indent[0] === '\n' && many([], t => t === indent && ++ pos && top())) || exp()
  const suffix = v => {
    switch (tokens[pos]) {
      case '.': return ++pos && suffix(['.', v, tokens[pos]])
      case '(': return ++pos && many([], t => t === ')' ? !++pos : exp())
      default: return v
    }
  }
  const atom = () => check(t => t === ':' && ++pos && block()) || suffix(value())
  const exp = () => (a => check(t => isOp2(t) && [t, a, ++pos && exp()]) || a)(atom())
  const top = () => many([], exp)
  return many([], top)
}
const generate = nodes => {
  const js = node => {
    if (isArray(node)) {
      if (node.length === 0) { fail('empty node', {nodes}) }
      const head = node[0]
      if (head === 'def') {
        return 'const ' + node[1] + ' = ' + '(' + node.slice(2, -1).join(',') + ') => ' + js(node[node.length - 1])
      } else {
        return node
      }
    } else {
      return node
    }
  }
  return nodes.map(js).join('\n')
}

const test = () => {
  const t = (expect, exp, ...defs) => {
    const src = defs.concat('def main: ' + exp).join('\n')
    const actual = run(src)
    if (eq(expect, actual)) {
      write('.')
    } else {
      puts('src:', src)
      puts('expect:', expect)
      puts('actual:', actual)
    }
  }
  t(1, '1')
  puts('ok')
}

test()
