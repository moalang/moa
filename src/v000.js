const str = o => JSON.stringify(o)
const put = s => process.stdout.write(s)
const puts = (...args) => console.log(...args)
const debug = (...args) => console.log(...args)
const assert = (cond, obj) => { if (!cond) { console.trace('Assert: ', obj) } }
const newToken = (tag, val) => ({tag, val})
const reserved = ['enum', 'if', 'do', 'case']


function escapeReservedWord(val) {
  return reserved.includes(val) ? '_' + val : val
}

function tokenize(source) {
  // source helpers
  let remaining = source
  const moveForward = n => { remaining = remaining.slice(n) }
  const match = (tag, r, f) => {
    const m = remaining.match(r)
    if (m) {
      moveForward(m[0].length)
      const v = m[1] || m[0]
      const val = f ? f(v, m) : v
      return newToken(tag, escapeReservedWord(val))
    }
  }
  const some = (tag, str) => {
    for (const val of str.split(' ')) {
      if (remaining.startsWith(val)) {
        moveForward(val.length)
        return newToken(tag, val)
      }
    }
  }

  // tokenize
  const readToken = () => {
    remaining = remaining.replace(/^[ \n\r\t]+/, '')
    return some('open1', '(') ||
      some('close1', ')') ||
      some('op2', '+ - * % / => , . : || | && &') ||
      some('func', '=') ||
      some('bool', 'true false') ||
      match('comment', /^#[^\r\n]*/) ||
      match('num', /^(\d+(?:\.\d+)?)/) ||
      match('str', /^"[^"]*"/) ||
      match('id', /^[a-z_][a-zA-Z0-9_]*/)
  }
  let tokens = []
  while (true) {
    const token = readToken()
    if (token) {
      if (token.tag !== 'comment') {
        tokens.push(token)
      }
    } else {
      assert(!remaining, {token, remaining, source , tokens})
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
      assert(expectTag || token.tag !== expectTag, {expectTag, token})
      if (token.tag === 'open1') { ++nest }
      if (token.tag === 'close1') { --nest }
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
    const l = parseCall(parseValue(token))
    if (look().tag === 'op2') {
      const op2 = consume('op2').val
      const r = parseCall(parseExp(consume()))
      if (op2 === ',') {
        return '[' + l + ',' + r + ']'
      } else if (op2 === ':') {
        return '{' + l + ':' + r + '}'
      } else {
        return l + op2 + r
      }
    } else {
      return l
    }
  }
  const parseCall = (node) => {
    if (look().tag === 'open1') {
      consume('open1')
      return parseCall(node + parseArguments())
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
      args.push(parseExp(token))
    }
    return '(' + args.join(', ') + ')'
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
  const parseValue = (token) => {
    switch (token.tag) {
    case 'num':
    case 'bool':
    case 'str':
      return token.val
    case 'id':
      return token.val
    case 'open1':
      return parseOpen1()
    default:
      throw err('Invalid close tag', token, index)
    }
  }
  const parseDefine = (token) => {
    assert(token.tag === 'id')

    const name = token.val
    const args = take(t => t.tag === 'id')
    consume('func')

    const body = parseExp(consume())
    return 'const ' + name + ' = ' + body
    //return 'function ' + name + '(' + args.join(',') + ') { return ' + body + ' }'
  }
  const parseTop = parseDefine

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
    actual = exec(js + "\nmain")
  } catch (e) {
    error = e.message
  }
  return { source, tokens, nodes, js, actual, error }
}

function exec(src) {
  const _ = {}
  const int = n => n
  const string = s => s
  function list(...args) {
    return args
  }
  function dict(...args) {
    let d = {}
    for (let i=0; i<args.length; i+=2) {
      d[args[i]] = args[i+1]
    }
    return d
  }
  function struct(...args) {
    let d = {}
    for (const arg of args) {
      for (const k of Object.keys(arg)) {
        d[k] = arg[k]
      }
    }
    return d
  }
  function __new(keys, vals) {
    let o = {}
    keys.forEach((key,i) => {
      o[key] = vals[i]
    })
    return o
  }
  function _enum(...tags) {
    function enumBody(i, o) {
      function match(...funcs) {
        return funcs[i](o)
      }
      return {match}
    }
    if (tags.length == 1 && typeof(tags[0]) === 'function') {
      const f = tags[0]
      const s = f()
      Object.keys(s).forEach((k,i) => {
        const v = s[k]
        if (v === undefined) {
          s[k] = a => enumBody(i, a)
        } else {
          s[k] = enumBody(i, v)
        }
      })
      return s
    } else {
      let d = {}
      tags.forEach((tag,i) => {
        for (const k of Object.keys(tag)) {
          const v = tag[k]
          if (v === int || v === string) {
            d[k] = a => enumBody(i, a)
          } else if (typeof(v) === "object") {
            d[k] = (...argv) => enumBody(i, __new(Object.keys(v), argv))
          } else {
            d[k] = enumBody(i, v)
          }
        }
      })
      return d
    }
  }
  function _if(...args) {
    for (let i=1; i<args.length; i+=2) {
      if (args[i-1]) {
        return args[i]
      }
    }
    return args[args.length-1]
  }
  function _case(v, ...args) {
    for (let i=0; i<args.length; i+=2) {
      if (v == args[i]) {
        return args[i+1]
      }
    }
    return args[args.length-1]
  }
  function _do(...args) {
    return args[args.length - 1]
  }
  return eval(src)
}

function tester(callback) {
  let errors = []
  function test(...args) {
    let [f, expect, source, ...funcs] = args
    funcs.push('main = ' + source)
    const result = run(funcs.join("\n"))
    if (str(expect) === str(f(result))) {
      put(".")
    } else {
      put("x")
      result.expect = expect
      errors.push(result)
    }
  }
  const eq = (...args) => test(x => x.actual, ...args)

  callback({eq})

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

function unitTests() {
  tester(t => {
    // primitives
    t.eq(1, "1")
    t.eq(1.2, "1.2")
    t.eq("hi", "\"hi\"")
    t.eq(true, "true")
    t.eq([1,2], "1,2")
    t.eq(1, "(a=>a)(1)")
    // container
    t.eq([1,2], 'list(1 2)')
    t.eq({1:2, 3:4}, 'dict(1 2 3 4)')
    // exp
    t.eq(5, "2 + 3")
    t.eq(-1, "2 - 3")
    t.eq(6, "2 * 3")
    t.eq(2, "6 / 3")
    // struct
    t.eq({a:1, b:2}, 'struct(a:1 b:2)')
    t.eq(2, 'struct(a:1 b:2).b')
    // enum
    t.eq(1, 'enum(a:int).a(1).match(a=>a)')
    t.eq({}, 'enum(a => struct(none:_ some:a)).none.match(a=>a b=>b)')
    t.eq(2, 'enum(a => struct(none:_ some:a)).some(2).match(a=>a b=>b)')
    t.eq(1, 'enum(v1:int v2:struct(x:int y:int)).v1(1).match(v1=>v1 v2=>v2.x+v2.y)')
    t.eq(3, 'enum(v1:int v2:struct(x:int y:int)).v2(1 2).match(v1=>v1 v2=>v2.x+v2.y)')
    // branch
    t.eq(1, 'if(true 1 2)')
    t.eq(2, 'if(false 1 2)')
    t.eq(2, 'if(false 1 true 2 3)')
    t.eq(3, 'if(false 1 false 2 3)')
    t.eq(1, 'case(1 1 1 2 2 3 3 _ 9)')
    t.eq(2, 'case(2 1 1 2 2 3 3 _ 9)')
    t.eq(3, 'case(3 1 1 2 2 3 3 _ 9)')
    t.eq(9, 'case(4 1 1 2 2 3 3 _ 9)')
    // statement
    t.eq(1, 'do(1)')
    t.eq(2, 'do(1 2)')
    t.eq(3, 'do(1 2 3)')
    // definition
    t.eq(3, 'a+b', 'a=1', 'b=2')
    // buildin
  /*
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
  })
}

function integrationTests() {
  const fs = require('fs')
  const src = fs.readFileSync('v001.moa', 'utf8')
  tester(t => {
    t.eq(1, "compile(1)", src)
  })
}

function main(command) {
  if (command === "test") {
    process.exit(unitTests() + integrationTests())
  } else {
    var input = require('fs').readFileSync('/dev/stdin', 'utf8');
    puts(input)
  }
}

main(process.argv[2])