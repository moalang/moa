'use strict'
function compile(source) {
  function isInfix(s) {
    return '+-*/%<>!=|&^?.'.includes(s[0])
  }
  function tokenize() {
    let pos = 0
    let lineno = 1
    return source.split(/(".*?"|[(){}\[\].]|[\r\n\t ]+)/g).map(code => {
      const o = {code, pos, lineno}
      pos += code.length
      lineno += code.split('\n').length - 1
      return o
    }).filter(x => x.code.trim().length)
  }
  function parse(tokens) {
    let pos = 0
    function line() {
      const lineno = tokens[pos].lineno
      const a = loop(t => t.lineno === lineno && !(')]}'.includes(t.code)), consume)
      return a.length === 1 ? a[0] : a
    }
    function consume() {
      return suffix(tokens[pos++])
    }
    function suffix(token) {
      if (pos >= tokens.length) {
        return token
      } else if (isInfix(tokens[pos].code)) {
        return suffix([tokens[pos++], token, consume()])
      } else if (token.code === '(') {
        const a = until(')')
        if (a.length !== 1) {
          throw new Error(`Unsupported '${JSON.stringify(a)}' around ${JSON.stringify(token)}`)
        }
        return suffix(a[0])
      } else if (token.code === '{') {
        token.lines = loop(t => t.code !== '}' || pos++ === null, line)
        return token
      } else if (tokens[pos-1].pos + tokens[pos-1].code.length === tokens[pos].pos && tokens[pos].code === '(') {
        pos++
        return suffix([token, ...until(')')])
      } else {
        return token
      }
    }
    function until(c) {
      return loop(t => t.code !== c || ++pos === null, consume)
    }
    function loop(f, g) {
      const a = []
      while (pos < tokens.length && f(tokens[pos])) {
        a.push(g())
      }
      return a
    }
    return loop(_ => true, line)
  }
  const reserved = 'return continue break'.split(' ')
  function gen(node) {
    if (Array.isArray(node)) {
      if (node[0].code === 'def') {
        return `const ${gen(node[1])} = (${node.slice(2, -1).map(gen).join(', ')}) => ${ gen(node.at(-1)) }`
      } else if (node[0].code === 'var') {
        return `let ${gen(node[1])} = ${node.length === 3 ? gen(node[2]) : gen(node.slice(2))}`
      } else if (isInfix(node[0].code)) {
        return gen(node[1]) + node[0].code + gen(node[2])
      } else {
        return `${gen(node[0])}(${node.slice(1).map(gen).join(', ')})`
      }
    } else if (/^[A-Za-z_]/.test(node.code) && !reserved.includes(node.code)) {
      return '_' + node.code
    } else if (node.code[0] == '{') {
      return '{' + node.lines.map(gen).join(';\n') + '}'
    } else {
      return node.code
    }
  }
  return parse(tokenize()).map(gen).join('\n\n')
}

if (process.argv[2] === 'test') {
  function eq(expect, source) {
    const js = compile(source)
    const actual = eval(compile(source))
    if (expect == actual) {
      process.stdout.write('.')
    } else {
      throw new Error(`${expect} != ${actual}\n${source}\n${js}`)
    }
  }
  eq(1, '1')
  eq(2, 'def inc n n + 1\ninc(1)')
  eq(1, 'def calc { var n 0\n n += 1\n return n }\ncalc()')
  console.log('ok')
} else {
  console.log(eval(compile(require('fs').readFileSync(0, 'utf-8'))))
}
