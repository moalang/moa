const str = o => JSON.stringify(o)
const put = s => process.stdout.write(s)
const puts = (...args) => console.log(...args)
const title = (t, ...args) => { put(t); puts(...args) }
const debug = (...args) => console.dir(args.length===1 ? args[0] : args, {depth: null})
const fs = require('fs')
const selfLines = fs.readFileSync('v0.js', 'utf8').split('\n')
const vm1 = fs.readFileSync('vm1.js', 'utf8') + "\n"
const vm0 = fs.readFileSync('vm0.js', 'utf8') + "\n"
const moa = fs.readFileSync('v1.moa', 'utf8') + "\n"

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
  function until(token, code) {
    const vals = []
    while (true) {
      const val = read()
      assert(!val.eot, ({pos:token.pos,source,code,pos,val,vals,tokens}))
      vals.push(val)
      if (val.code === code) {
        return vals
      }
    }
  }
  function read() {
    let token = consume()
    if (token.eot || token.code === ']' || token.code === ')') {
      return token
    }
    if (look().type) {
      assert(token.id, token)
      const type = consume()
      type.name = token.code
      return type
    }

    if (token.code === '(') {
      const tokens = until(token, ')')
      assert(tokens.length === 2, tokens)
      token.body = tokens[0]
    } else if (token.code === '[') {
      token.array = until(token, ']').slice(0, -1)
    } else if (token.id && look().code === '(' && look().pos === token.pos + token.code.length) {
      consume()
      token.args = until(token, ')').slice(0, -1)
      token.name = token.code
    }

    if (look().def) {
      const sym = consume()
      const rhs = read()
      sym.name = token.code
      sym.body = rhs
      token = sym
    }

    while (look().op2 || look().eff) {
      const sym = consume()
      sym.lhs = token
      sym.rhs = read()
      if (sym.eff || sym.code === '<-') {
        sym.name = token.code
      }
      token = sym
    }

    return token
  }

  let nodes = []
  while (pos < tokens.length) {
    nodes.push(read())
  }

  return nodes
}

function generate(nodes) {
  function call(node,f) {
    const funcs = node.args.filter(x => x.def).map(gen)
    const vals = node.args.filter(x => !x.def)

    if (node.code === 'do') {
      const rec = body => {
        const val = vals.shift()
        if (val && val.code === '<-') {
          const arg = val.name
          return '_bind(' + gen(val.rhs) + ', ' + arg + ' => ' + rec(arg) + ')'
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

    const name = node.name === 'if' ? '_if' : node.name
    const body = name + (vals ? '(' + vals.map(gen).join(',') + ')' : '')
    if (funcs.length > 0) {
      return '(function() {\n' + funcs.join("\n") + '\nreturn ' + body + '})()'
    } else {
      return body
    }
  }
  function gen(node) {
    if (node.code === '[') {
      return '[' + node.array.map(gen).join(',') + ']'
    } else if (node.code === '(') {
      return '(' + gen(node.body) + ')'
    } else if (node.type) {
      return 'const ' + node.name + ' = _type(' + str(node.code) + ')'
    } else if (node.code == '=') {
      return 'const ' + node.name + ' = ' + gen(node.body)
    } else if (node.code == ':=') {
      return 'let ' + node.name + ' = new _var(' + gen(node.body) + ')'
    } else if (node.op2 && node.code === '=>') {
      return '((' + gen(node.lhs) + ') => ' + gen(node.rhs) + ')'
    } else if (node.op2 && node.code === '.') {
      return gen(node.lhs) + '.' + gen(node.rhs)
    } else if (node.op2) {
      return '_op2("' + node.code + '",' + gen(node.lhs) + ',' + gen(node.rhs) + ')'
    } else if (node.eff) {
      return node.name + '.eff("' + node.code + '",' + gen(node.rhs) + ')'
    } else if (node.args) {
      return call(node)
    } else if (node.num || node.str || node.id) {
      return node.code
    } else {
      assert(false, node)
    }
  }
  return nodes.map(gen).join("\n")
}

function exec(js) {
  // evaluate source in sandbox
  js = vm0 + js + '\n_top(main)'
  try {
    return eval(js)
  } catch (e) {
    const lines = js.split('\n')
    printStack(e, lines)
    for (const [index, line] of lines.entries()) {
      puts((index+1).toString().padStart(3, ' ') + ':', line)
    }
    process.exit(2)
    throw e
  }
}

function tester(callback) {
  function test(f, expect, ...funcs) {
    const src = funcs.join("\n")
    const tokens = tokenize(src)
    const nodes = parse(tokens, src)
    const js = generate(nodes, src)
    const output = exec(js)
    const actual = f(output)
    if (str(expect) === str(actual)) {
      put(".")
    } else {
      title("expect: ", str(expect))
      title("actual: ", str(actual))
      title("output: ", output)
      puts("javascript:")
      for (const [i, line] of js.split('\n').entries()) {
        puts((i+1).toString().padStart(3, ' ') + ':', line)
      }
      puts("source:")
      puts(src)
      process.exit(3)
    }
  }
  function eq(expect, source, ...funcs) {
    funcs.push('main = ' + source)
    return test(x => x, expect, ...funcs)
  }
  function eq2(expect, source, ...funcs) {
    const src = funcs.concat(["main = " + source]).join("\n")
    const ret = s => 'return ' + (s.startsWith('error: ') ? str(s) : s)
    const run = js => {
      js += "\nreturn main"
      try {
        return Function(js)()
      } catch (e) {
        debug({src, js})
        throw e
      }
    }
    return test(x => run(vm1 + x), expect, moa, 'main = compile(' + str(src) + ')')
  }
  callback({eq,eq2})
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
  tester(t => {
    t.eq2(1, '1')
    t.eq2(1, '(1)')
    t.eq2(9, '(1 + 2) * 3')
    t.eq2(7, '1 + (2 * 3)')
    t.eq2('hi', '"hi"')
    t.eq2('hi', ' "h" ++ "i" ')
    t.eq2(3, 'a+b', 'a=1', 'b=2')
    t.eq2(3, 'inc(2)', 'inc = x => x + 1')
    t.eq2(1, 'do(1)')
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
