const str = o => JSON.stringify(o)
const put = s => process.stdout.write(s)
const puts = (...args) => console.log(...args)
const assert = (cond, ...args) => { if (!cond) { console.trace('Assert: ', args) } }
const newToken = (tag, val) => ({tag, val})


function tokenize(source) {
  // source helpers
  let remaining = source
  const advance = n => { remaining = remaining.slice(n) }
  const match = (tag, r, f) => {
    const m = remaining.match(r)
    if (m) {
      advance(m[0].length)
      const v = m[1] || m[0]
      const val = f ? f(v, m) : v
      return newToken(tag, val)
    }
  }
  const some = (tag, str) => {
    for (const val of str.split(' ')) {
      if (remaining.startsWith(val)) {
        advance(val.length)
        return newToken(tag, val)
      }
    }
  }

  // tokenize
  const readToken = () => {
    remaining = remaining.replace(/^[ \t]+/, '')
    return some('open1', '(') ||
      some('close1', ')') ||
      some('open2', '[') ||
      some('close2', ']') ||
      some('open3', '{') ||
      some('close3', '}') ||
      some('arrow', '=>') ||
      some('branch', '\n|') ||
      some('func', '=') ||
      some('as', ':') ||
      some('op2', '+ - * % / , || | && &') ||
      some('bool', 'true false') ||
      match('num', /^(\d+(?:\.\d+)?)/) ||
      match('str', /^"[^"]*"/) ||
      match('id', /^[a-z_][a-zA-Z0-9_]*/) ||
      match('br', /^[\r\n]([ \t\r\n])*/, _ => '\n')
  }
  let tokens = []
  while (true) {
    const token = readToken()
    if (token) {
      tokens.push(token)
    } else {
      assert(!remaining, token, remaining, source , '=>', tokens)
      return tokens
    }
  }
}

function parse(tokens) {
  let index = 0
  let nest1 = 0
  let nest2 = 0
  let nest3 = 0
  const len = tokens.length
  const err = (...args) => Error(JSON.stringify(args))
  const look = () => {
    if (index < len) {
      return tokens[index]
    } else {
      return {}
    }
  }
  const consume = () => {
    if (index < len) {
      const token = tokens[index]
           if (token.tag === 'open1') { ++nest1 }
      else if (token.tag === 'open2') { ++nest2 }
      else if (token.tag === 'open3') { ++nest3 }
      else if (token.tag === 'close1') { --nest1 }
      else if (token.tag === 'close2') { --nest2 }
      else if (token.tag === 'close3') { --nest3 }
      ++index
      return token
    } else {
      throw err('EOT', tokens)
    }
  }
  const take = f => {
    let result = []
    while (true) {
      const token = look()
      if (f(token)) {
        consume()
        result.push(token.val)
      } else {
        return result
      }
    }
  }
  const until = f => take(t => !f(t))

  const parseExp = (token) => {
    return parseExpRemaining(parseOp2(token))
  }
  const parseOp2 = (token) => {
    const l = parseValue(token)
    if (look().tag === 'op2') {
      const op2 = consume().val
      const r = parseOp2(consume())
      return l + op2 + r
    } else {
      return l
    }
  }
  const parseExpRemaining = (node) => {
    const tag = look().tag
    if (tag === 'branch') {
      let conds = []
      while (look().tag === 'branch') {
        consume()
        const cond = parseValue(consume())
        consume()
        const exp = parseOp2(consume())
        if (cond === '_') {
          conds.push('return ' + exp)
        } else {
          conds.push('if (__branch === ' + cond + ') return ' + exp)
        }
      }
      return '(function(__branch){\n  ' + conds.join('\n  ') + '\n})(' + node + ')'
    } else {
      return node
    }
  }
  const parseOpen1 = (baseNest) => {
    const vals = until(t => baseNest === nest1 && t.tag === 'close1')
    consume()
    if (vals.length === 0) {
      return undefined
    } else if (vals.length === 1) {
      return vals[0]
    } else {
      const border = vals.findIndex(val => val == '=>')
      if (border >= 0) {
        let args = vals.slice(0, border)
        let body = vals.slice(border + 1)
        return '((' + args.join('') + ') => ' + body.join('') + ')'
      }
    }

    if (vals[1] === ':') {
      return '({' + vals.join(', ').replace(/, :, /g, ':') + '})'
    } else {
      return '[' + vals.join(', ') + ']'
    }
  }
  const parseOpen2 = (baseNest) => {
    const vals = until(t => baseNest === nest2 && t.tag === 'close2')
    consume()
    return '[' + vals.join(', ').replace(', ]', '') + ']'
  }
  const parseOpen3 = (baseNest) => {
    const vals = until(t => baseNest === nest3 && t.tag === 'close3')
    consume()
    const len = vals.length
    if (len >= 2 && vals[1] == ':') {
      let kvs = []
      for (let i=0; i<len; i+=3) {
        kvs.push(vals[i]+':'+vals[i+2])
      }
      return '({' + kvs.join(',') + '})'
    } else {
      let kvs = []
      for (let i=0; i<len; i+=2) {
        kvs.push(vals[i]+':'+vals[i+1])
      }
      return '({' + kvs.join(',') + '})'
    }
  }
  const parseArguments = (baseNest) => {
    const vals = until(t => baseNest === nest1 && t.tag === 'close1')
    consume()
    return '(' + vals.join(', ') + ')'
  }
  const parseValue = (token) => {
    return tryValue(token) || (() => { throw err('parseValue', token) })()
  }
  const tryValue = (token) => {
    switch (token.tag) {
    case 'num':
    case 'bool':
    case 'str':
      return token.val
    case 'id':
      if (look().tag === 'open1') {
        const baseNest = nest1
        _ = consume()
        let argv = []
        while (true) {
          const t = consume()
          if (t.tag === 'close1' && baseNest === nest1) {
            return token.val + '(' + argv.join(', ') + ')'
          }

          const val = parseExp(t)
          argv.push(val)
        }
        const vals = until(t => baseNest === nest1 && t.tag === 'close1')
      } else {
        return token.val
      }
    case 'open1':
      const node = parseOpen1(nest1)
      if (look().tag === 'open1') {
        consume()
        return node + parseArguments(nest1)
      } else {
        return node
      }
    case 'open2':
      return parseOpen2(nest2)
    case 'open3':
      return parseOpen3(nest3)
    case 'close1':
      return ')'
    case 'close2':
      return ']'
    case 'close3':
      return '}'
    }
  }
  const parseStmt = (token) => {
    switch (token.tag) {
    case 'id':
      const name = token.val
      const args = take(t => t.tag === 'id')
      const next = look()
      if (args.length === 0 && next.tag === 'open1') {
        return parseExp(token)
      }
      if (next.tag === 'func') {
        consume()
        const body = parseExp(consume())
        return 'function ' + name + '(' + args.join(',') + ') { return ' + body + ' }'
      } else {
        assert(args.length === 0)
        return name
      }
    default:
      return parseExp(token)
    }
  }
  const parseTop = parseStmt

  let nodes = []
  while (true) {
    if (index >= len) {
      return nodes
    }
    const token = consume()
    if (token.tag === 'br') {
      continue
    }
    const node = parseTop(token)
    nodes.push(node)
  }
}

function run(source) {
  const tokens = tokenize(source)
  const nodes = parse(tokens)
  const js = nodes.join("\n")
  let stdout = ''
  let actual
  let error
  try {
    const print = x => { stdout += x }
    actual = eval(js)
  } catch (e) {
    error = e
  }
  return { source, tokens, nodes, js, actual, stdout, error }
}

function run_test() {
  let errors = []
  function test(expect, source, f) {
    const result = run(source)
    if (str(expect) === str(f(result))) {
      put(".")
    } else {
      put("x")
      result.expect = expect
      errors.push(result)
    }
  }
  function eq(expect, source) {
    return test(expect, source, x => x.actual)
  }
  function stdout(expect, source) {
    return test(expect, source, x => x.stdout)
  }

  // primitives
  eq(1, "1")
  eq(1.2, "1.2")
  eq("hi", "\"hi\"")
  eq(true, "true")
  // container
  eq([1, 2, 3], "[1 2 3]")
  eq([1, 2], "(1 2)")
  eq({1: 1, a: 2}, "(1:1 a:2)") // struct
  eq({1: 1, a: 2}, "{1 1 a 2}") // dictionary(any any)
  eq({x: 1, y: 2}, "{x:1 y:2}") // dictionary(stirng any)
  // closure
  eq(3, "(a,b => a + b)(1 2)")
  // exp
  eq(5, "2 + 3")
  eq(-1, "2 - 3")
  eq(6, "2 * 3")
  eq(2, "6 / 3")
  // branch
  eq(1, "1\n| 1 = 1\n| 2 = 2")
  eq(2, "2\n| 1 = 1\n| 2 = 2")
  eq(3, "3\n| 1 = 1\n| _ = 3")
  // statement
  stdout('hello', 'print("hello")')
  // function
  eq(2, "inc a = a + 1\ninc(1)")
  eq(6, "add a b = a + b\nadd(1 2 + 3)")
/*
  -- exp(8)
  test "1" "ab enum:\n  a\n  b\nab.a\n| a = 1\n| b = 2"
  test "2" "ab enum:\n  a\n  b\nab.b\n| a = 1\n| b = 2"
  -- container(5)
  test "1" "[1 2](0)"
  test "5" "[1 2+3](1)"
  test "5" "[1, 2+3](1)"
  test "5" "[1, 2+3].n1"
  test "1" "s class: n int, m int\ns(1 2).n"
  test "3" "ab enum:\n  a x int\n  b y int\nab.a(3).x"
  test "4" "ab enum:\n  a x int\n  b y int\nab.b(4).y"
  -- error(2)
  test "error: divide by zero" "1 / 0"
  test "2" "1 / 0 | 2"
  -- built-in
  test "1" "\"01\".to_i"
  test "1,2,3" "[1 2 3].map(x -> x.to_s).join(\",\")"
  -- bugs
  test "1" "id x = x\nid1 x = id(x)\nid1(1)"
  putStrLn "done"
*/

  puts(errors.length === 0 ? "ok" : "FAILED")
  errors.forEach(e => {
    puts("source: " + e.source)
    puts("expect: " + str(e.expect))
    puts("actual: " + str(e.actual))
    puts("stdout: " + str(e.stdout))
    puts("error : " + str(e.error))
    puts("js    : " + str(e.js))
  })
  return errors.length
}

function main(command) {
  if (command === "test") {
    process.exit(run_test())
  } else {
    var input = require('fs').readFileSync('/dev/stdin', 'utf8');
    puts(input)
  }
}

main(process.argv[2])
