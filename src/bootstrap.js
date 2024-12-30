'use strict'
function compile(source) {
  function isInfix(s) {
    return '+-*/%<>!=|&^?.'.includes(s[0])
  }
  function tokenize() {
    let pos = 0
    let lineno = 1
    return source.split(/([(){}\[\].]|[\r\n\t ]+)/g).map(code => {
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
      return loop(t => t.lineno === lineno, consume)
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
        return suffix(until(')'))
      } else if (tokens[pos-1].pos + tokens[pos-1].code.length === tokens[pos].pos && tokens[pos].code === '(') {
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
  function gen(node) {
    if (Array.isArray(node)) {
      if (node[0].code === 'def') {
        return `func _${node[1].code}(${node.slice(2, -1).map(arg => arg.code + ' ' + gotype(arg)).join(', ')}) { ${ gen(node.at(-1)) } }`
      } else if (isInfix(node[0].code)) {
        return gen(node[1]) + node[0].code + gen(node[2])
      } else if (node.length === 1) {
        return gen(node[0])
      } else {
        return `${gen(node[0])}(${node.slice(1).map(gen).join(', ')})`
      }
    } else if (/^[A-Za-z_]/.test(node.code[0])) {
      return '_' + node.code
    } else {
      return node.code
    }
  }
  return parse(tokenize()).map(gen).join('\n\n')
}

const std = require('fs').readFileSync(__dirname + '/std.go', 'utf-8')
const user = compile(require('fs').readFileSync(0, 'utf-8'))
console.log(std + user)
