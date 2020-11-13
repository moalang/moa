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
  let nest = 0
  const len = tokens.length
  const err = (...args) => Error(JSON.stringify(args))
  const look = () => index < len ? tokens[index] : {}
  const consume = (expectTag) => {
    if (index < len) {
      const token = tokens[index]
      assert(expectTag || token.tag !== expectTag, expectTag, token)
      if (['open1', 'open2', 'open3'].includes(token.tag)) { ++nest }
      if (['close1', 'close2', 'close3'].includes(token.tag)) { --nest }
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
  const untilClose = (tag) => {
    const base = nest
    const vals = take(t => !(base === nest && t.tag === tag))
    consume(tag)
    return vals
  }
  const parseExp = (token) => {
    return parseIfCall(parseOp2(token))
  }
  const parseOp2 = (token) => {
    const l = parseValue(token)
    if (look().tag === 'op2') {
      const op2 = consume('op2').val
      const r = parseOp2(consume())
      return l + op2 + r
    } else {
      return l
    }
  }
  const parseIfCall = (node) => {
    if (look().tag === 'open1') {
      consume('open1')
      return parseIfCall(node + parseArguments())
    } else {
      return node
    }
  }
  const parseArguments = () => {
    const baseNest = nest - 1
    const args = []
    while (true) {
      const token = consume()
      if (baseNest === nest && token.tag === 'close1') {
        break
      }
      const exp = parseOp2(token)
      args.push(exp)
    }
    return '(' + args.join(', ') + ')'
  }
  const parseIfBranch = (node) => {
    const tag = look().tag
    if (tag === 'branch') {
      let conds = []
      while (look().tag === 'branch') {
        consume('branch')
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
  const parseOpen1 = () => {
    const vals = untilClose('close1')
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
  const parseOpen2 = () => {
    const vals = untilClose('close2')
    return '[' + vals.join(', ').replace(', ]', '') + ']'
  }
  const parseOpen3 = () => {
    const vals = untilClose('close3')
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
  const parseValue = (token) => {
    switch (token.tag) {
    case 'num':
    case 'bool':
    case 'str':
      return token.val
    case 'id':
      return parseIfCall(token.val)
    case 'open1':
      return parseIfCall(parseOpen1())
    case 'open2':
      return parseIfCall(parseOpen2())
    case 'open3':
      return parseIfCall(parseOpen3())
    case 'close1':
    case 'close2':
    case 'close3':
      throw err('Invalid close tag', token)
    }
  }
  const parseStmt = (token) => {
    assert(token.tag === 'id')

    const name = token.val
    const args = take(t => t.tag === 'id')
    consume('func')

    const body = parseIfBranch(parseExp(consume()))
    return 'function ' + name + '(' + args.join(',') + ') { return ' + body + ' }'
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
  let actual = null
  let error = null
  try {
    actual = eval(js + "\nmain()")
  } catch (e) {
    error = e
  }
  return { source, tokens, nodes, js, actual, error }
}

function run_test() {
  let errors = []
  function test(...args) {
    const [f, expect, source, ...funcs] = args
    const extra = funcs.map(x => "\n" + x).join()
    const result = run("main = " + source + extra)
    if (str(expect) === str(f(result))) {
      put(".")
    } else {
      put("x")
      result.expect = expect
      errors.push(result)
    }
  }
  const eq = (...args) => test(x => x.actual, ...args)

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
  // effect (omit)
  // function
  eq(2, "inc(1)", "inc a = a + 1")
  eq(6, "add(1 2 + 3)", "add a b = a + b")
  // enum
  // struct (TBD)
  // buildin (TBD)
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
  for (const e of errors) {
    puts("source: " + e.source)
    puts("expect: " + str(e.expect))
    puts("actual: " + str(e.actual))
    puts("stdout: " + str(e.stdout))
    puts("error : " + str(e.error))
    puts("js    : " + str(e.js))
  }
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
