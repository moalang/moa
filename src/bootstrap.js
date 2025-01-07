'use strict'
const std = (function() {
  let ___test_ok = 0
  let ___test_failed = 0
  const _io = {
    _in: {
      get _string() {
        return require('fs').readFileSync(0, 'utf-8')
      }
    },
    _puts(...a) {
      console.log(...a)
    },
    _warn(...a) {
      console.warn(...a)
    },
    _argv: process.argv.slice(2)
  }
  function _throw(obj) {
    const e = new Error(obj)
    e._data = obj
    throw e
  }
  function _list(...a) {
    return a
  }
  function _map(...a) {
    return new Map([... new Array(a.length / 2)].map((_,i) => [a[i*2], a[i*2+1]]))
  }
  function _test(f) {
    function _eq(a, b) {
      if (JSON.stringify(a) === JSON.stringify(b)) {
        ___test_ok++
      } else {
        ___test_failed++
      }
    }
    f({_eq})
  }
  function prop(o, field) {
    const type = Array.isArray(o) ? 'array' : typeof o
    switch (type + field) {
      case 'string_size': return o.length
      case 'string_int': return parseInt(o)
      case 'string_at': return i => o[i]
      case 'string_has': return s => o.includes(s)
      case 'string_slice': return (...a) => o.slice(...a)
      case 'string_split': return s => o.split(s)
      case 'number_string': return o.toString()
      case 'array_size': return o.length
      case 'array_at': return n => o.at(n)
      case 'array_map': return f => o.map(f)
      case 'array_push': return x => o.push(x)
      case 'array_join': return s => o.map(x => x.toString()).join(s)
      case 'array_has': return s => o.includes(s)
      default:
        if (typeof o === 'object' && field in o) {
          return o[field]
        } else {
          throw new Error(`No '${field}' of '${type}'`)
        }
    }
  }
}).toString().slice(14, -2) + '\n\n\n'

function compile(source) {
  function isInfixLeft(s) {
    return '.'.includes(s[0])
  }
  function isInfixRight(s) {
    return '+-*/%<>!=|&^?'.includes(s[0])
  }
  function tokenize() {
    let pos = 0
    let lineno = 1
    return source.split(/("(?:[^"\\]|\\.)*?"|[(){}\[\].]|[\r\n\t ]+)/g).map(code => {
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
      } else if (isInfixLeft(tokens[pos].code)) {
        const op2 = tokens[pos++]
        return suffix([op2, token, tokens[pos++]])
      } else if (isInfixRight(tokens[pos].code)) {
        return suffix([tokens[pos++], token, consume()])
      } else if (token.code === '(') {
        const a = until(')')
        if (a.length !== 1) {
          throw new Error(`Unsupported '${JSON.stringify(a)}' around ${JSON.stringify(token)}`)
        }
        token.body = a[0]
        return suffix(token)
      } else if (token.code === '[') {
        return suffix([{code: 'list', pos: token.pos}, ...until(']')])
      } else if (token.code === '{') {
        token.lines = loop(t => t.code !== '}' || pos++ === null, line)
        return token
      } else if (tokens[pos-1].pos + tokens[pos-1].code.length === tokens[pos].pos && tokens[pos].code === '(') {
        pos++
        return suffix([token, ...until(')')])
      } else if (tokens[pos-1].pos + tokens[pos-1].code.length === tokens[pos].pos && tokens[pos].code === '[') {
        pos++
        const index = consume()
        pos++
        return suffix([[{code: '.'}, token, {code: 'at'}], index])
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
      if (Array.isArray(node[0])) {
        return node.length === 1 ? gen(node[0]) : `${gen(node[0])}(${node.slice(1).map(gen).join(', ')})`
      } else if (node[0].code === 'def') {
        return `const ${gen(node[1])} = (${node.slice(2, -1).map(gen).join(', ')}) => ${ gen(node.at(-1)) }`
      } if (node[0].code === 'class') {
        const fields = node.at(-1).lines.map(a => a[0].code).join(', ')
        return `function ${gen(node[1])}(${fields}) { return {${fields}}}`
      } if (node[0].code === 'if') {
        return `if (${gen(node[1])}) ${gen(node[2])}`
      } if (node[0].code === 'else') {
        return node.length === 2 ? `else ${gen(node[1])}` : `else ${gen(node.slice(1))}`
      } if (node[0].code === 'catch') {
        return `(() => {try { return ${gen(node[1])} } catch (__e) { return (${gen(node[2])})(__e) }})()`
      } else if (['var', 'let'].includes(node[0].code)) {
        return `let ${gen(node[1])} = ${node.length === 3 ? gen(node[2]) : gen(node.slice(2))}`
      } else if (node[0].code === 'iif') {
        function iif(a) {
          return a.length === 1 ? gen(a[0]) : gen(a[0]) + '?' + gen(a[1]) + ':' + iif(a.slice(2))
        }
        return iif(node.slice(1))
      } else if (node[0].code === 'for') {
        const ijk = node.length >= 4 ? gen(node[2]) : '_'
        const step = node.length >= 5 ? gen(node[3]) : 1
        return `for (let ${ijk}=0; ${ijk}<${gen(node[1])}; ${ijk}+=${step}) { ${gen(node.at(-1))} }`
      } else if (node[0].code === 'each') {
        const item = node.length >= 4 ? gen(node[2]) : '_'
        const ijk = node.length >= 5 ? gen(node[3]) : 'i'
        return `const __a = ${gen(node[1])}; for (let ${ijk}=0; ${ijk}<__a.length; ${ijk}++) { let ${item} = __a[${ijk}]; ${gen(node.at(-1))} }`
      } else if (node[0].code === 'while') {
        const cond = node.length === 3 ? gen(node[1]) : gen(node.slice(1, -1))
        return `while (${cond}) ${gen(node.at(-1))}`
      } else if (node[0].code === '.' && /^[A-Za-z_]/.test(node[2].code)) {
        return `prop(${gen(node[1])}, '${gen(node[2])}')`
      } else if (isInfixLeft(node[0].code) || isInfixRight(node[0].code) ) {
        return `${gen(node[1])}${node[0].code}${gen(node[2])}`
      } else {
        return `${gen(node[0])}(${node.slice(1).map(gen).join(', ')})`
      }
    } else if (['true', 'false', 'else'].includes(node.code)) {
      return node.code
    } else if (/^[A-Za-z_]/.test(node.code) && !reserved.includes(node.code)) {
      return '_' + node.code
    } else if (node.code[0] === '(') {
      return '(' + gen(node.body) + ')'
    } else if (node.code[0] === '{') {
      return '{' + node.lines.map(gen).join('\n') + '}'
    } else {
      return node.code
    }
  }
  return parse(tokenize()).map(gen).join('\n')
}

if (process.argv[2] === 'test') {
  function tryEval(s) {
    try {
      return eval(s)
    } catch (e) {
      return e
    }
  }
  function eq(expect, source, f=tryEval) {
    const js = compile(source)
    const actual = f(std + js)
    if (JSON.stringify(expect) === JSON.stringify(actual)) {
      process.stdout.write('.')
    } else {
      throw new Error(`${expect} != ${actual}\n--\n${source}\n--\n${js}`)
    }
  }
  function err(expect, source) {
    eq(expect, source, s => tryEval(s)._data)
  }
  eq(2, 'def f { if false {\n return 1\n } else if true {\n return 2 \n} }\nf()')

  // primitive
  eq(true, 'true')
  eq('hi', '"hi"')
  eq('h"i', '"h\\"i"')
  eq('\\', '"\\\\"')
  eq('package main\nimport "fmt"\nfunc main() {\n  fmt.Println("Hello World")\n}', '"package main\\nimport \\"fmt\\"\\nfunc main() {\\n  fmt.Println(\\"Hello World\\")\\n}"')
  eq(1, '1')
  eq(1.2, '1.2')
  eq(1, '(x => x)(1)')

  // container
  eq([1, 2], 'list 1 2')
  eq([2, 3], 'list(1 2).map x => x + 1')
  eq('1 2', 'list(1 2).join " "')
  eq(new Map([[1, true]]), 'map 1 true')

  // property
  eq(2, '"hi".size')
  eq(['a', 'b'], '"a b".split " "')
  eq(1, '"1".int.string.int')
  eq(2, 'list(1 2).at 1')
  eq(true, 'list(1).has 1')

  // declaration
  eq(1, 'let a 1\na')
  eq(3, 'var a 1\na += 2\na')
  eq(2, 'def inc n n + 1\ninc(1)')
  eq(1, 'def calc { var n 0\n n += 1\n return n }\ncalc()')
  eq(1, 'test t => { t.eq 1 1 }\n__test_ok')
  eq(1, 'test t => { t.eq 1 2 }\n__test_failed')
  eq({x:1, y:2}, 'class v2 { x int\n y int}\nv2(1 2)')

  // control flow
  eq(1, 'def f { if true { return 1 }\n return 2 }\nf()')
  eq(2, 'def f { if false { return 1 }\n return 2 }\nf()')
  eq(2, 'def f { if false { return 1 }\n return 2 }\nf()')
  eq(1, 'def f { if true { return 1 }\n else { return 2 } }\nf()')
  eq(2, 'def f { if false { return 1 }\n else { return 2 } }\nf()')
  eq(2, 'def f { if false { return 1 }\n else if true { return 2 } }\nf()')
  eq(2, 'def f { if false {\n return 1 \n}\n else if true {\n return 2 \n} }\nf()')
  eq(1, 'iif true 1 2')
  eq(2, 'iif false 1 2')
  eq(3, 'iif false 1 false 2 3')
  eq(3, 'var n 0\n for 3 { n += 1 }\n n')
  eq(3, 'var n 0\n for 3 i { n += i }\n n')
  eq(6, 'var n 0\n for 5 i 2 { n += i }\n n')
  eq(2, 'var n 0\n each list(2 3) { n += 1 }\n n')
  eq(5, 'var n 0\n each list(2 3) m { n += m }\n n')
  eq(3, 'var n 0\n each list(2 3) m i { n += i * m }\n n')
  eq(3, 'var n 0\n while n < 3 { n += 1 }\n n')
  eq(1, 'var n 0\n while n < 3 { n += 1\n if n == 1 break }\n n')
  eq(4, 'var n 0\n each list(1 2 3) m { if m == 2 continue \n n += m }\n n')

  // variable
  eq([1], 'var a list()\n a.push(1)\n a')

  // syntax sugar
  eq([1], '[1]')
  eq(2, '[1 2 3][1]')

  // error
  err(1, 'throw 1')
  eq(1, 'catch throw(1) e => e.data')

  console.log('ok')
} else if (process.argv[2] === 'js') {
  console.log(std + compile(require('fs').readFileSync(0, 'utf-8')) + '\n_main()')
} else {
  console.log(eval(std + compile(require('fs').readFileSync(0, 'utf-8')) + '\n_main()'))
}
