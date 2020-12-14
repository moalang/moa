const str = o => JSON.stringify(o)
const put = s => process.stdout.write(s)
const puts = (...args) => console.log(...args)
const title = (t, ...args) => { put(t); debug(...args) }
const debug = (...args) => console.dir(args.length===1 ? args[0] : args, {depth: null})
const escape = x => ['if', 'do'].includes(x) ? '_' + x : x
const any = (x,...xs) => xs.some(v => v === x)

function assert(cond, ...objs) {
  if (cond) {
    return
  }
  try {
    throw new Error()
  } catch (e) {
    const fs = require('fs')
    const src = fs.readFileSync('v000.js', 'utf8').split('\n')
    const rows = e.stack.split('\n').map(x => x.match(/(\d+):(\d+)/)).filter(x=>x)
    puts('Assertion failed')
    for (const row of rows.slice(1,6)) {
      const [_,line,column] = row
      puts(line + ': ' + src[line-1])
      puts(line + ': ' + ' '.repeat(column-1) + '^')
    }
    debug(...objs)
    process.exit(1)
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
    console.error('Faield: ' + js, e)
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
    some(p, 'eff', '+= -= *= /= <-') ||
    some(p, 'op2', '|| && == != >= <= ++ => > < + - * / , .') ||
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
  function local(argv,f) {
    assert(argv[0].call, argv)
    assert(argv.slice(-1)[0].code === ')', argv)
    argv = argv.slice(1, -1)
    const funcs = argv.filter(x => x.def)
    const vals = argv.filter(x => !x.def)
    if (funcs.length > 0) {
      const def = funcs.map(gen).join("\n")
      return '(function() {\n' + def + '\nreturn ' + f(vals) + '})()'
    } else {
      return f(vals)
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
        return local(node.argv, vals => id(node) + (vals ? '(' + vals.map(gen).join(',') + ')' : ''))
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
  function _if(...args) {
    for (let i=1; i<args.length; i+=2) {
      if (args[i-1]) {
        return args[i]
      }
    }
    return args[args.length-1]
  }
  function _do(...args) {
    let ret
    for (const arg of args) {
      ret = typeof(arg) === 'function' ? arg() : arg
    }
    return ret
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
    return typeof(exp) === 'function' ? exp : () => exp
  }
  function error(message, args) {
    try {
      throw new Error()
    } catch (e) {
      const eid = e.stack
      const obj = {message, args, eid}
      obj.catch = (target,alt) => target.unwrap().eid === eid ? alt : obj
      obj.unwrap = () => obj
      return obj
    }
  }

  // WARN: global pollution
  String.prototype.__defineGetter__('int', function() { return parseInt(this) })
  Number.prototype.__defineGetter__('string', function() { return this.toString() })
  Array.prototype.__defineGetter__('len', function() { return this.length })
  Function.prototype.valueOf = function() { return this.unwrap() }
  Function.prototype.unwrap = function() {
    let f = this
    while (typeof(f) === 'function') {
      f = f()
    }
    return f
  }
  Function.prototype.catch = function (e, alt) {
    e = e.unwrap()
    const f = this
    return (...args) => {
      const ret = f(...args).unwrap()
      const v = ret.eid === e.eid ? alt : ret
      return _lazy(v)
    }
  }
  function _top(f) {
    const ret = f.unwrap().valueOf()
    return ret.eid ? 'error: ' + ret.message : ret
  }
  function _op2(op, lhs, rhs) {
    return _lazy(() => {
      lhs = lhs.valueOf()
      rhs = rhs.valueOf()
      switch (op) {
        case '+' : return _lazy(lhs + rhs)
        case '-' : return _lazy(lhs - rhs)
        case '*' : return _lazy(lhs * rhs)
        case '/' : return rhs === 0 ? error('divide by zero', lhs, rhs) : _lazy(lhs / rhs)
        case '++' : return _lazy(lhs.concat(rhs))
        default:
          assert(false, {op,lhs,rhs})
      }
    })
  }

  // evaluate source in sandbox
  return eval(src + '\n_top(main)')
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
    puts("--")
    title("expect: ", e.expect)
    title("actual: ", e.actual)
    title("source: ", e.source)
    title("stdout: ", e.stdout)
    title("error : ", e.error)
    title("js    : ", e.js)
    //title("nodes : ", e.nodes)
    //title("tokens: ", e.tokens)
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
