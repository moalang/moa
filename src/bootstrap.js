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
        const a = until(')')
        if (a.length !== 1) {
          throw new Error(`Unsupported '${JSON.stringify(a)}' around ${JSON.stringify(token)}`)
        }
        return suffix(a[0])
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
function run(source) {
  const std = require('fs').readFileSync(__dirname + '/std.go', 'utf-8')
  const user = compile(source)
  require('fs').writeFileSync('/tmp/a.go', std + '\n' + user)
  return require('child_process').execSync('go run /tmp/a.go', {encoding: 'utf-8'})
}

if (process.argv[2] === 'test') {
  function eq(expect, source) {
    const actual = run(source)
    if (expect == run(source)) {
      process.stdout.write('.')
    } else {
      throw new Error(`${expect} != ${actual}\n${source}`)
    }
  }
  eq('1', 'def main io.put(1)')
  eq('1\n', 'def main io.puts(1)')
  console.log('ok')
} else {
  console.log(run(require('fs').readFileSync(0, 'utf-8')))
}
