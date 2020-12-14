const str = o => JSON.stringify(o)
const put = s => process.stdout.write(s)
const puts = (...args) => console.log(...args)
const trace = (...args) => { console.log(...args); return true }
const title = (t, ...args) => { put(t); puts(...args) }
const debug = (...args) => console.dir(args.length===1 ? args[0] : args, {depth: null})
const escape = x => ['if', 'do'].includes(x) ? '_' + x : x
const any = (x,...xs) => xs.some(v => v === x)
const fs = require('fs')
const selfLines = fs.readFileSync('v0.js', 'utf8').split('\n')
const vm = fs.readFileSync('vm.js', 'utf8')

function assert(cond, ...objs) {
  if (cond) {
    return
  }
  try {
    throw new Error()
  } catch (e) {
    puts('Assertion failed')
    printStack(e, '')
    debug(...objs)
    process.exit(1)
  }
}

function printStack(e, js) {
  js = js ? js : selfLines
  const rows = e.stack.split('\n').map(x => x.match(/.+:(\d+):(\d+)/)).filter(x=>x)
  puts(e.message)
  for (const row of rows.slice(0,10)) {
    const [str,line,column] = row
    if (str.includes('<anonymous>')) {
      puts(line + ': ' + js[line-1])
    } else {
      puts(line + ': ' + selfLines[line-1])
    }
    puts(line + ': ' + ' '.repeat(column-1) + '^')
  }
}

function run(source) {
  let tokens, nodes, js, actual, error
  try {
    tokens = tokenize(source)
    nodes = parse(tokens, source)
    js = generate(nodes, source)
    actual = exec(js)
  } catch(e) {
    error = e.message
  }
  return { source, tokens, nodes, js, actual, error }
}

function tokenize(src) {
  const check = (pos,tag,m) => m ? ({tag, pos, code: typeof(m) === 'string' ? m : m[0]}) : null
  const match = (p,tag,r) => check(p, tag, src.slice(p).match(r))
  const some = (p,tag,s) => check(p, tag, s.split(' ').find(w => src.slice(p).startsWith(w)))
  const eat = p => match(p, 'num', /^[0-9]+(\.[0-9]+)?/) ||
    match(p, 'id', /^[A-Za-z_][A-Za-z0-9_]*([\.,][A-Za-z_][A-Za-z0-9_]*)*/) ||
    match(p, 'str', /^"(?:(?:\\")|[^"])*"/) ||
    match(p, 'ignore', /^[ #\n]+/) ||
    match(p, 'type', /^:\|? .+/) ||
    some(p, 'group', '[ ] ( )') ||
    some(p, 'eff', '+= -= *= /=') ||
    some(p, 'op2', '|| && == != >= <= ++ => <- > < + - * / , .') ||
    some(p, 'def', ':= =')

  let pos=0, tokens=[]
  while (pos < src.length) {
    const token = eat(pos)
    assert(token, pos)
    pos += token.code.length
    token[token.tag] = 1
    const prev = tokens.slice(-1)[0] || {}
    if (token.code === '(' && (prev.id || any(prev.code, ')', ']'))) {
      token.call = true
    }
    tokens.push(token)
  }

  const dst = tokens.map(t => t.code).join("")
  assert(src === dst, src, dst)
  return tokens.filter(t => !t.ignore)
}

function parse(tokens, source) {
  let pos = 0
  const eot = ({code:'',eot:1})
  const consume = () => tokens[pos++] || eot
  const look = () => tokens[pos] || eot
  function until(code) {
    const vals = []
    while (true) {
      const val = read()
      assert(!val.eot, ({source,code,pos,val,vals,tokens}))
      vals.push(val)
      if (val.code === code) {
        return vals
      }
    }
  }
  function read() {
    let token = consume()
    if (token.eot || any(token.code, ']', ')')) {
      return token
    }
    if (look().type) {
      assert(token.id, token)
      const type = consume()
      type.argv = [token]
      return type
    }

    if (token.code === '(') {
      token.argv = until(')')
    } else if (token.code === '[') {
      token.argv = until(']')
    } else if (token.id && look().call) {
      const open = consume()
      token.argv = [open].concat(until(')'))
    }

    if (look().def) {
      const sym = consume()
      const rhs = read()
      sym.argv = [token, rhs]
      token = sym
    }

    while (look().op2 || look().eff) {
      const sym = consume()
      const rhs = read()
      sym.argv = [token, rhs]
      token = sym
    }

    return token
  }

  let nodes = []
  while (pos < tokens.length) {
    nodes.push(read())
  }

  const src = tokens.map(t => t.code).join('  ')
  const tags = tokens.map(t => t.tag + '(' + t.code + ')').join(' ')
  const composedTokens = []
  const q = nodes.slice()
  while (q.length) {
    const node = q.pop()
    composedTokens.push(node)
    q.push(...(node.argv || []))
  }
  const dst = composedTokens.sort((a,b) => a.pos - b.pos).map(t => t.code).join('  ')
  assert(src === dst, {src,dst,tags})

  return nodes
}

function generate(nodes) {
  function id(node) {
    assert(node.id, node)
    return escape(node.code)
  }
  function local(node,f) {
    assert(node.argv[0].call, node.argv)
    assert(node.argv.slice(-1)[0].code === ')', node.argv)
    const argv = node.argv.slice(1, -1)
    const funcs = argv.filter(x => x.def).map(gen)
    const vals = argv.filter(x => !x.def)

    if (node.code === 'do') {
      const rec = body => {
        const val = vals.shift()
        if (val && val.code === '<-') {
          const arg = id(val.argv[0])
          return '_bind(' + gen(val.argv[1]) + ', ' + arg + ' => ' + rec(arg) + ')'
        } else if(val) {
          const arg = '_b'
          return '_bind(' + gen(val) + ', ' +  arg + ' => ' + rec(arg) + ')'
        } else {
          return body
        }
      }

      const body = rec('undefined')
      if (funcs.length > 0) {
        return '(function() {\n' + funcs.join("\n") + '\nreturn ' + body + '})'
      } else {
        return '() => ' + body
      }
    }

    const body = id(node) + (vals ? '(' + vals.map(gen).join(',') + ')' : '')
    if (funcs.length > 0) {
      return '(function() {\n' + funcs.join("\n") + '\nreturn ' + body + '})()'
    } else {
      return body
    }
  }
  function gen(node) {
    try {
      if (node.code === '[') {
        return '[' + node.argv.slice(0, -1).map(gen).join(',') + ']'
      } else if (node.code === '(') {
        assert(node.argv.length === 2, node)
        return '(' + gen(node.argv[0]) + ')'
      } else if (node.type) {
        return 'const ' + id(node.argv[0]) + ' = _type(' + str(node.code) + ')'
      } else if (node.code == '=') {
        return 'const ' + id(node.argv[0]) + ' = ' + gen(node.argv[1])
      } else if (node.code == ':=') {
        return 'let ' + id(node.argv[0]) + ' = new _var(' + gen(node.argv[1]) + ')'
      } else if (node.op2 && node.code === '=>') {
        return '((' + gen(node.argv[0]) + ') => ' + gen(node.argv[1]) + ')'
      } else if (node.op2 && node.code === '.') {
        return gen(node.argv[0]) + '.' + gen(node.argv[1])
      } else if (node.op2) {
        return '_op2("' + node.code + '",' + gen(node.argv[0]) + ',' + gen(node.argv[1]) + ')'
      } else if (node.eff) {
        return id(node.argv[0]) + '.eff("' + node.code + '",' + gen(node.argv[1]) + ')'
      } else if (node.argv) {
        return local(node)
      } else {
        return node.code
      }
    } catch (e) {
      console.error(e, node)
      throw e
    }
  }
  return nodes.map(gen).join("\n")
}

function exec(src) {
  // evaluate source in sandbox
  const js = vm + "\n" + src + '\n_top(main)'
  try {
    return eval(js)
  } catch (e) {
    const lines = js.split('\n')
    printStack(e, lines)
    for (const [index, line] of lines.entries()) {
      puts((index+1).toString().padStart(3, ' ') + ':', line)
    }
    throw e
  }
}

function tester(callback) {
  function test(...args) {
    let [f, expect, source, ...funcs] = args
    funcs.push('main = ' + source)
    const result = run(funcs.join("\n"))
    if (str(expect) === str(f(result))) {
      put(".")
    } else {
      title("expect: ", str(expect))
      title("actual: ", result.actual)
      title("source: ", result.source)
      title("stdout: ", result.stdout)
      title("error : ", result.error)
      for (const [i, line] of result.js.split('\n').entries()) {
        puts((i+1).toString().padStart(3, ' ') + ':', line)
      }
      process.exit(2)
    }
  }
  const eq = (...args) => test(x => x.actual, ...args)

  callback({eq})

  puts("ok")
}

function unitTests() {
  tester(t => {
    // primitives
    t.eq(1, "1")
    t.eq(1.2, "1.2")
    t.eq("hi", "\"hi\"")
    t.eq('hi"world', '"hi\\"world"')
    t.eq(true, "true")
    t.eq(1, "f(1)", 'f = a => a')
    t.eq(3, "f(1 2)", 'f = a,b=>a+b')
    // container
    t.eq([1,2], '[1 2]')
    // exp
    t.eq(5, "2 + 3")
    t.eq(-1, "2 - 3")
    t.eq(6, "2 * 3")
    t.eq(2, "6 / 3")
    t.eq(14, "2 + (3 * 4)")
    t.eq(20, "(2 + 3) * 4")
    t.eq(10, "(2 * 3) + 4")
    t.eq(14, "2 * (3 + 4)")
    // definition
    t.eq(3, 'a+b', 'a=1', 'b=2')
    // struct
    t.eq({a:1, b:[]}, 'ab(1 [])', 'ab: a int, b [int]')
    t.eq([2,3], 'ab(1 [2 3]).b', 'ab: a int, b [int]')
    // enum
    t.eq(1, 'ast(ast.int(1) v=>v _)', 'ast:| int int | op2: op string, lhs ast, rhs ast')
    t.eq(3, 'ast(ast.op2("+" 1 2) _ o=>o.lhs+o.rhs)', 'ast:| int int | op2: op string, lhs ast, rhs ast')
    // branch
    t.eq(1, 'if(true 1 2)')
    t.eq(2, 'if(false 1 2)')
    t.eq(2, 'if(false 1 true 2 3)')
    t.eq(3, 'if(false 1 false 2 3)')
    // statement
    t.eq(1, 'do(1)')
    t.eq(2, 'do(1 2)')
    t.eq(3, 'do(1 2 3)')
    t.eq(0, 'do(n := 0 n)')
    t.eq(1, 'do(n := 0 n+=1 n)')
    t.eq(2, 'do(n := 0 f=n+=1 f f n)')
    // error handling
    t.eq("error: divide by zero", "1 / 0")
    t.eq("error: nil", "nil", 'nil = error("nil")')
    t.eq("error: nil", "do(nil)", 'nil = error("nil")')
    t.eq(1, "do(nil).catch(nil 1)", 'nil = error("nil")')
    t.eq(2, "b.catch(a 1).catch(b 2)", 'a = error("msg")', 'b = error("msg")')
    // built-in
    t.eq(11, '"11".int')
    t.eq('1,2', '[1 2].map(x => x.string).join(",")')
    t.eq(2, '[1 2].len')
    t.eq([1,2], '[1]++[2]')
  })
}

function integrationTests() {
  const fs = require('fs')
  const src = fs.readFileSync('v1.moa', 'utf8')
  tester(t => {
    t.eq(1, 'compile(1)', src)
    t.eq('error: miss', 'tokenize("")', src)
    t.eq('id', 'do(t <- tokenize("id") t.val)', src)
    t.eq('str', 'do(t <- tokenize("\\"str\\"") t.val)', src)
    t.eq(123, 'do(t <- tokenize("123") t.val)', src)
  })
}

function main(command) {
  if (command === "test") {
    unitTests()
    integrationTests()
  } else {
    var source = require('fs').readFileSync('/dev/stdin', 'utf8');
    const tokens = tokenize(source)
    const nodes = parse(tokens, source)
    const js = generate(nodes, source)
    const main = "const _in = require('fs').readFileSync('/dev/stdin', 'utf8');" + 'console.log(compile(_in))'
    puts(vm + "\n" + js + "\n" + main)
  }
}

main(process.argv[2])
