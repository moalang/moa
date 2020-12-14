const str = o => JSON.stringify(o)
const put = s => process.stdout.write(s)
const puts = (...args) => console.log(...args)
const title = (t, ...args) => { put(t); puts(...args) }
const debug = (...args) => console.dir(args.length===1 ? args[0] : args, {depth: null})
const escape = x => ['if', 'do'].includes(x) ? '_' + x : x
const any = (x,...xs) => xs.some(v => v === x)
const fs = require('fs')
const selfLines = fs.readFileSync('v000.js', 'utf8').split('\n')

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
    match(p, 'str', /^"[^"]*?"/) ||
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
        return '(function() {\n' + funcs.join("\n") + '\nreturn ' + body + '})()'
      } else {
        return body
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
        return 'const ' + id(node.argv[0]) + ' = _lazy(' + gen(node.argv[1]) + ')'
      } else if (node.code == ':=') {
        return 'let ' + id(node.argv[0]) + ' = new _eff(' + gen(node.argv[1]) + ')'
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
  const _ = {}
  function _dot(v, obj) {
    assert(v !== undefined, obj)
    return v
  }
  function _if(...args) {
    assert(args.length % 2 === 1, args)
    for (let i=1; i<args.length; i+=2) {
      if (args[i-1].valueOf()) {
        return args[i].valueOf()
      }
    }
    return args[args.length-1]
  }
  function _div(lhs, rhs) {
    if (rhs === 0) {
      return error('divide by zero', lhs, rhs)
    }
    return lhs / rhs
  }
  function _type(line) {
    if (line.startsWith(': ')) {
      const keys = line.slice(1).split(',').map(x => x.trim().split(' ')[0])
      return (...vals) => keys.reduce((o,k,i) => ({...o, [k]: vals[i]}), {})
    } else if (line.startsWith(':| ')) {
      const f = (x,...args) => args[x.index](x.val)
      const fields = line.slice(2).split('|').map(f => f.trim())
      for (const [index, field] of fields.entries()) {
        if (field.match(/^\w+$/)) {
          f[field] = {index}
        } else if (field.match(/^\w+ \w+/)) {
          const tag = field.split(' ')[0]
          f[tag] = val => ({index,val})
        } else if (field.includes(':')) {
          const [tag, names] = field.split(': ')
          const keys = names.split(',').map(x => x.trim().split(' ')[0])
          f[tag] = (...vals) => ({index, val:keys.reduce((o,k,i) => ({...o, [k]: vals[i]}), {})})
        } else {
          assert(false, line)
        }
      }
      return f
    } else {
      assert(false, line)
    }
  }
  function _eff(val) {
    this.val = val
  }
  _eff.prototype.eff = function(op, rhs) {
    rhs = rhs.valueOf()
    switch(op) {
      case '+=': return () => this.val += rhs
      case '-=': return () => this.val -= rhs
      case '*=': return () => this.val *= rhs
      case '/=': return () => this.val = _div(this.val, rhs)
      case '<-': return () => this.val = _do(rhs)
    }
  }
  _eff.prototype.valueOf = function() {
    return this.val
  }
  _eff.prototype.isEff = true
  function _lazy(exp) {
    return (...args) => (typeof(exp) === 'function' ? () => exp(...args) : () => exp)
  }
  function error(message, args) {
    try {
      throw new Error()
    } catch (e) {
      const eid = e.stack
      const obj = {message, args, eid}
      obj.catch = (target,alt) => target.valueOf().eid === eid ? alt : obj
      obj.valueOf = () => obj
      return obj
    }
  }

  // WARN: global pollution
  String.prototype.__defineGetter__('int', function() { return parseInt(this) })
  String.prototype.__defineGetter__('len', function() { return this.length })
  Number.prototype.__defineGetter__('string', function() { return this.toString() })
  Array.prototype.__defineGetter__('len', function() { return this.length })
  Function.prototype.valueOf = function() {
    let f = this
    while (typeof(f) === 'function') {
      f = f()
    }
    return f
  }
  Function.prototype.catch = function (e, alt) {
    e = e.valueOf()
    const f = this
    return (...args) => {
      const ret = f(...args).valueOf()
      const v = ret.eid === e.eid ? alt : ret
      return _lazy(v)
    }
  }
  function _bind(obj, f) {
    return _lazy(() => {
      const ret = obj.valueOf()
      if (ret.eid) {
        return ret
      }
      return f(ret)
    })
  }
  function _op2(op, lhs, rhs) {
    return _lazy(() => {
      lhs = lhs.valueOf()
      rhs = rhs.valueOf()
      switch (op) {
        case '+' : return _lazy(lhs + rhs)
        case '-' : return _lazy(lhs - rhs)
        case '*' : return _lazy(lhs * rhs)
        case '/' : return _lazy(_div(lhs, rhs))
        case '++' : return _lazy(lhs.concat(rhs))
        case '<=' : return _lazy(lhs <= rhs)
        case '>=' : return _lazy(lhs >= rhs)
        case '&&' : return _lazy(lhs && rhs)
        case '||' : return _lazy(lhs || rhs)
        case '<' : return _lazy(lhs < rhs)
        case '>' : return _lazy(lhs > rhs)
        default:
          assert(false, {op,lhs,rhs})
      }
    })
  }
  function _top(f) {
    const ret = f.valueOf()
    return ret.eid ? 'error: ' + ret.message : ret
  }

  // evaluate source in sandbox
  const js = src + '\n_top(main)'
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
      title("expect: ", result.expect)
      title("actual: ", result.actual)
      title("source: ", result.source)
      title("stdout: ", result.stdout)
      title("error : ", result.error)
      title("js    : ", result.js)
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
  const src = fs.readFileSync('v001.moa', 'utf8')
  tester(t => {
    t.eq(1, 'compile(1)', src)
    t.eq('error: miss', 'tokenize("")', src)
    t.eq('hi', 'tokenize("hi")', src)
  })
}

function main(command) {
  if (command === "test") {
    unitTests()
    integrationTests()
  } else {
    var input = require('fs').readFileSync('/dev/stdin', 'utf8');
    puts(input)
  }
}

main(process.argv[2])
