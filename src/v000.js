const str = o => JSON.stringify(o)
const put = s => process.stdout.write(s)
const puts = (...args) => console.log(...args)
const debug = (...args) => console.log(...args)
const assert = (cond, obj) => { if (!cond) { console.trace('Assert: ', obj) } }
const reserved = ['if', 'do', 'case', 'var']

function escapeReservedWord(val) {
  return reserved.includes(val) ? '_' + val : val
}

function tokenize(source) {
  // source helpers
  let remaining = source
  let line = 1
  let column = 1
  const newToken = (tag, val) => ({tag, val, line, column})
  const moveForward = n => {
    for (const c of remaining.slice(0, n)) {
      if (c === '\n') {
          line+=1
          column=1
      } else {
        column++
      }
    }
    remaining = remaining.slice(n)
  }
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

  const readToken = () => {
    moveForward(remaining.match(/^[ \n\r\t]*/)[0].length)
    return some('open1', '(') ||
      some('close1', ')') ||
      some('open2', '[') ||
      some('close2', ']') ||
      some('op2', '+= -= *= /= := + - * % / => <- . || | && &') ||
      some('func', '=') ||
      some('bool', 'true false') ||
      match('type', /^[a-zA-Z_][a-zA-Z0-9_]*:\|? *[^\n]+/) ||
      match('comment', /^#[^\r\n]*/) ||
      match('num', /^(\d+(?:\.\d+)?)/) ||
      match('str', /^"[^"]*"/) ||
      match('group', /^[a-z_][a-zA-Z0-9_]*(?:, *[a-z_][a-z[A-Z0-9_]*)+/) ||
      match('id', /^[a-zA-Z_][a-zA-Z0-9_]*/)
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
  const newLiteral = val => ({tag: 'literal', val})
  const newOp2 = (op2,lhs,rhs) => ({tag: 'op2', val:{op2,lhs,rhs}})
  const newCall = (node, argv) => ({tag: 'call', val:{node,argv}})
  const newDefine = (id, node) => ({tag: 'define', val:{id,node}})
  const len = tokens.length
  const err = (...args) => {
    console.error(args, {remaining: tokens.slice(index)})
    return Error(JSON.stringify(args))
  }
  const look = () => index < len ? tokens[index] : {}
  const consume = (expectTag) => {
    if (index < len) {
      const token = tokens[index]
      assert(expectTag || token.tag !== expectTag, {expectTag, token})
      if (token.tag === 'open1') { ++nest }
      if (token.tag === 'close1') { --nest }
      if (token.tag === 'open2') { ++nest }
      if (token.tag === 'close2') { --nest }
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
      return newOp2(op2, l, r)
    } else {
      return l
    }
  }
  const parseCall = (node) => {
    if (look().tag === 'open1') {
      consume('open1')
      return parseCall(newCall(node, readArguments()))
    } else {
      return node
    }
  }
  const readArguments = () => {
    const baseNest = nest - 1
    const args = []
    while (true) {
      const token = consume()
      if (baseNest === nest && token.tag === 'close1') {
        break
      }
      args.push(parseExp(token))
    }
    return args
  }
  const parseOpen1 = () => {
    const vals = untilClose('close1')
    if (vals.length === 0) {
      throw err('Empty ()')
    } else if (vals.length === 1) {
      // TODO: remove this check
      if (vals[0].includes(",")) {
        return newLiteral("{" + vals[0] + "}")
      } else {
        return newLiteral(vals[0])
      }
    } else {
      const arrowPos = vals.findIndex(val => val == '=>')
      if (arrowPos >= 0) {
        let args = vals.slice(0, arrowPos)
        let body = vals.slice(arrowPos + 1)
        return newLiteral('((' + args.join('') + ') => ' + body.join('') + ')')
      }
    }
  }
  const parseOpen2 = () => {
    const vals = untilClose('close2')
    return newLiteral("[" + vals.join(",") + "]")
  }
  const parseValue = (token) => {
    switch (token.tag) {
    case 'num':
    case 'bool':
    case 'str':
      return newLiteral(token.val)
    case 'id':
      return newLiteral(token.val)
    case 'group':
      return newLiteral('{' + token.val + '}')
    case 'open1':
      return parseOpen1()
    case 'open2':
      return parseOpen2()
    default:
      throw err('Invalid close tag', token.tag, index)
    }
  }
  const parseDefine = (token) => {
    if (token.tag === 'type') {
      const pos = token.val.indexOf(':')
      const name = token.val.slice(0, pos).trim()
      const line = token.val.slice(pos + 1).trim()
      const isEnum = token.val.includes(':|')
      if (isEnum) {
        function enumToKV(def, index) {
          const makeNew = (tag, args, fields) => '((' + args + ") => ({tag:'" + tag + "', index: " + (index - 1) + ', ' + fields + '}))'
          if (def.includes(':')) {
            const [tag, struct] = def.split(':')
            const args = struct.split(',').map(x => x.trim().split(' ')[0]).join(',')
            return tag + ':' + makeNew(tag, args, 'val:{' + args + '}')
          } else {
            const [tag, _] = def.trim().split(' ')
            return tag + ':' + makeNew(tag, 'val', 'val')
          }
        }
        const [_, ...tags] = line.split('|').map(x => x.trim()).map(enumToKV)
        const branch = '(x,...branches) => branches[x.index](x.val)'
        const func = 'Object.assign(' + branch + ', {' + tags.join(',') + '})'
        return newDefine(name, newLiteral(func))
      } else {
        const fields = line.split(',').map(x => x.trim().split(' ')[0]).join(',')
        const constructor = '((' + fields + ') => ({' + fields + '}))'
        return newDefine(name, newLiteral(constructor))
      }

    } else {
      assert(token.tag === 'id')

      const name = token.val
      const args = take(t => t.tag === 'id')
      consume('func')

      const body = parseExp(consume())
      return newDefine(name, body)
    }
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
  return nodes
}

function generate(nodes) {
  function gen({tag,val}) {
    switch (tag) {
      case 'literal': return val
      case 'call': return gen(val.node) + (val.argv ? '(' + val.argv.map(gen).join(',') + ')' : '')
      case 'define': return 'const ' + val.id + ' = ' + gen(val.node)
      case 'op2':
        const op2 = val.op2
        const lhs = gen(val.lhs)
        const rhs = gen(val.rhs)
        switch (op2) {
          case '<-': return lhs + ' = ' + rhs
          default: return lhs + op2 + rhs
        }
    }
  }
  const defines = []
  for (const node of nodes) {
    assert(node.tag === 'define', node)
    defines.push(gen(node))
  }
  return defines.join('\n')
}

function run(source) {
  const tokens = tokenize(source)
  const nodes = parse(tokens)
  const js = generate(nodes)
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
  const list = (...x) => x
  const _var = v => v
  function __new(keys, vals) {
    let o = {}
    keys.forEach((key,i) => {
      o[key] = vals[i]
    })
    return o
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
    puts("expect: " + str(e.expect))
    puts("actual: " + str(e.actual))
    puts("source: " + e.source)
    puts("stdout: " + str(e.stdout))
    puts("error : " + str(e.error))
    puts("js    : " + str(e.js))
    console.log("tokens:", e.tokens)
    put("nodes: ")
    console.dir(e.nodes, {depth: null})
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
    t.eq(1, "(a=>a)(1)")
    // container
    t.eq([1,2], '[1 2]')
    // exp
    t.eq(5, "2 + 3")
    t.eq(-1, "2 - 3")
    t.eq(6, "2 * 3")
    t.eq(2, "6 / 3")
    // struct
    t.eq({a:1, b:2}, 'ab(1 2)', 'ab: a int, b int')
    t.eq(2, 'ab(1 2).b', 'ab: a int, b int')
    // enum
    t.eq(1, 'ast(ast.int(1) v=>v _)', 'ast:| int int | op2: op string, lhs ast, rhs ast')
    t.eq(2, 'ast(ast.op2("+" 1 2) _ o=>o.rhs)', 'ast:| int int | op2: op string, lhs ast, rhs ast')
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
    t.eq(0, 'do(n <- var(0) n)')
    t.eq(1, 'do(n <- var(0) n+=1 n)')
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
