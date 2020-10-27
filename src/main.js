const str = o => JSON.stringify(o)
const put = s => process.stdout.write(s)
const puts = (...args) => console.log(...args)
const assert = (cond, ...args) => { if (!cond) { console.error('Assert: ', args) } }
const newToken = (tag, val) => ({tag, val})


function tokenize(source) {
  // source helpers
  let remaining = source
  const consume = n => { remaining = remaining.slice(n) }
  const match = (tag, r, f) => {
    const m = remaining.match(r)
    if (m) {
      consume(m[0].length)
      const v = m[1] || m[0]
      const val = f ? f(v, m) : v
      return newToken(tag, val)
    }
  }
  const some = (tag, xs) => {
    for(let i=0; i<xs.length; ++i) {
      const val = xs[i]
      if (remaining.startsWith(val)) {
        consume(val.length)
        return newToken(tag, val)
      }
    }
  }
  const eq = (tag, x) => {
    if (remaining.startsWith(x)) {
      consume(x.length)
      return newToken(tag, x)
    }
  }

  // tokenize
  const read_token = () => {
    remaining = remaining.replace(/^[ \t]+/, '')
    return eq('open1', '(') ||
      eq('close1', ')') ||
      eq('open2', '[') ||
      eq('close2', ']') ||
      some('func', '='.split(' ')) ||
      some('op2', '+ - * % / , || | && &'.split(' ')) ||
      some('bool', 'true false'.split(' ')) ||
      match('num', /^(\d+(?:\.\d+)?)/) ||
      match('str', /^"[^"]*"/) ||
      match('id', /^[a-z_][a-zA-Z0-9_]*/) ||
      match('br', /^[\r\n]([ \t\r\n])*/, _ => '\n')
  }
  let tokens = []
  while (true) {
    const token = read_token()
    if (token) {
      tokens.push(token)
    } else {
      assert(!remaining, remaining, source , '=>', tokens)
      return tokens
    }
  }
}

function parse(tokens) {
  let index = 0
  let nest1 = 0
  let nest2 = 0
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
      if (token.tag === 'open1') {
        ++nest1
      } else if (token.tag === 'open2') {
        ++nest2
      } else if (token.tag === 'close1') {
        --nest1
      } else if (token.tag === 'close2') {
        --nest2
      }
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
    const l = parseValue(token)
    if (look().tag === 'op2') {
      const op2 = consume().val
      const r = parseExp(consume())
      return l + op2 + r
    } else {
      return l
    }
  }
  const parseOpen1 = (baseNest) => {
      const vals1 = until(t => baseNest === nest1 && t.tag === 'close1')
      consume()
      if (vals1.length === 1) {
        return vals1[0]
      } else {
        return '[' + vals1.join(' ') + ']'
      }
  }
  const parseOpen2 = (baseNest) => {
      const vals2 = until(t => baseNest === nest2 && t.tag === 'close2')
      consume()
      return '[' + vals2.join(', ').replace(', ]', '') + ']'
  }
  const parseValue = (token) => {
    return tryValue(token) || (() => { throw err('parseExp', token) })()
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
      return parseOpen1(nest1)
    case 'open2':
      return parseOpen2(nest2)
    case 'close1':
      return ')'
    case 'close2':
      return ']'
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
  try {
    return eval(js)
  } catch (e) {
    puts('-- source:', source)
    puts('-- tokens:', tokens)
    puts('-- nodes:', nodes)
    puts('-- js:', js)
  }
}

function run_test() {
  let errors = []
  function eq(expect, source) {
    const actual = run(source)
    if (str(expect) === str(actual)) {
      put(".")
    } else {
      put("x")
      errors.push({expect, actual, source})
    }
  }

  //// value
  eq(1, "1")
  eq(1.2, "1.2")
  eq("hi", "\"hi\"")
  eq(true, "true")
  // container
  eq([1, 2, 3], "[1 2 3]")
  //eq([1, 2], "(1, 2)")
  //eq({x: 1, y: 2}, "(x=1, y=2)")
  //eq({1: true, 2: false}, "{1 true, 2 false}")
  // exp
  eq(5, "2 + 3")
  eq(-1, "2 - 3")
  eq(6, "2 * 3")
  eq(2, "6 / 3")
  eq(2, "inc a = a + 1\ninc(1)")
  eq(6, "add a b = a + b\nadd(1 2 + 3)")
/*

  -- value(4)
  test "1" "1"
  test "hello world" "\"hello world\""
  test "true" "true"
  test "false" "false"
  test "2" "inc a = a + 1\ninc(1)"
  test "6" "add a b = a + b\nadd(1 2 + 3)"
  -- exp(8)
  test "3" "1 + 2"
  test "-1" "1 - 2"
  test "6" "2 * 3"
  test "4" "9 / 2"
  test "1" "a = 1\na"
  test "3" "c = 1\nb n = n + c\na = b(2)\na"
  test "2" "a = 1\nincr = a += 1\nincr\na"
  test "2" "a =\n  1\n  2\nb = a; a\nc = b\nc"
  test "1" "true\n| 1\n| 2"
  test "2" "false\n| 1\n| 2"
  test "true" "1\n| 1 = true\n| 2 = false"
  test "false" "2\n| 1 = true\n| 2 = false"
  test "false" "3\n| 1 = true\n| _ = false"
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
