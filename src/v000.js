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
    throw e
  }
}

function run(source) {
  let tokens, nodes, js, actual, error
  try {
    tokens = tokenize(source)
    nodes = parse(tokens,source)
    js = generate(nodes,source)
    actual = exec(js + "\nmain").valueOf()
  } catch(e) {
    error = e.message
  }
  return { source, tokens, nodes, js, actual, error }
}

function tokenize(source) {
  const codes = source.match(/(\s+|\d+(?:\.\d+)?|[a-zA-Z_]\w*(?:\.[a-zA-Z_]\w*)*|"[^\"]*"|[+\-*/:]=|:\||=>|<-|(?:\|\|)|&&|[,\.+\-*/:=()\[\]\|]|#[^\n]*)/g)
  const src = str(source)
  const dst = str(codes.join(""))
  assert(src == dst, src, dst)
  let line = 1
  let column = 1
  let prev = {}
  let tokens = []
  for (const code of codes) {
    const c = code[0]
    let token = ({code,column,line})
    for (const c of code) {
      if (c === '\n') {
        line++
        column=1
      } else {
        column++
      }
    }
    const tag = ('a' <= c && c <= 'z') ? 'id' :
                ('0' <= c && c <= '9') ? 'num' :
                (c === '"') ? 'str' :
                any(c, ' ', '#', '\n') ? 'ignore' :
                any(code, 'true', 'false') ? 'bool' :
                any(c, '(', ')', '[', ']') ? 'group' :
                any(code, '+', '-', '*', '/', ',', '.', '=>', '||', '&&') ? 'op2' :
                any(code, '+=', '-=', '*=', '/=') ? 'eff' :
                any(code, '=', ':=') ? 'def' :
                any(code, ':', ':|') ? 'type' :
                null
    token[tag] = true
    if (!token.ignore) {
      if (token.code === '(' && (prev.id || any(prev.code, ')', ']'))) {
        token.call = true
      }
      tokens.push(token)
    }
    prev = token
  }
  return tokens
}

function parse(tokens,source) {
  let pos = 0
  const eot = ({code:'',column:0,line:0,eot:true})
  const consume = () => tokens[pos++] || eot
  const look = () => tokens[pos] || eot
  function readLine(token) {
    const line = token.line
    const vals = []
    while (true) {
      const val = consume()
      if (val.line !== line) {
        pos--
        return vals
      }
      vals.push(val)
    }
  }
  function until(code) {
    const vals = []
    while (true) {
      const val = read()
      assert(!val.eot, ({source,code,pos,val,vals,tokens}))
      if (val.code === code) {
        return vals
      }
      vals.push(val)
    }
  }
  function split(tokens, code) {
    const all = []
    let parts = []
    for (const token of tokens) {
      if (token.code === code) {
        if (parts.length > 0) {
          all.push(parts)
          parts = []
        }
      } else {
        parts.push(token)
      }
    }
    if (parts.length > 0) {
      all.push(parts)
    }
    return all
  }
  function read() {
    let token = consume()
    if (token.eot) {
      return token
    }

    if (token.code === '(') {
      const exps = until(')')
      assert(exps.length === 1, exps)
      token.argv = exps
    } else if (token.code === '[') {
      token.argv = until(']')
      return token
    } else if (token.group) {
      return token
    } else if (token.id) {
      if (look().call) {
        consume()
        token.argv = until(')')
      } else if (look().type) {
        const [sym, ...tokens] = readLine(token)
        if (sym.code === ':') {
          sym.argv = [token].concat(tokens.filter((_,index) => index % 3 == 0))
        } else if (sym.code === ':|') {
          sym.argv = [token].concat(split(tokens, '|'))
        } else {
          assert(false, sym)
        }
        return sym
      }
    }

    while (look().op2 || look().eff || look().def) {
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
  return nodes
}

function generate(nodes) {
  function id(node) {
    assert(node.id, node)
    return escape(node.code)
  }
  function local(argv,f) {
    const funcs = argv.filter(x => x.def)
    const vals = argv.filter(x => !x.def)
    if (funcs.length > 0) {
      const def = funcs.map(gen).join("\n")
      return '(function() {\n' + def + '\nreturn ' + f(vals) + '})()'
    } else {
      return f(vals)
    }
  }
  function enumFields(tags) {
    function enumField(fields, index) {
      const [head, ...items] = fields
      const tag = 'f.' + head.code + ' = '
      if (items.length === 0) {
        return tag + index
      } else if (items.length === 1) {
        return tag + '(val=>({val, _index:' + index + '}))'
      } else {
        const ids = items.filter((_,index) => index % 3 === 1).map(id).join(',')
        return tag + '((' + ids + ') => ({val:{' + ids + '}, _index:' + index + '}))'
      }
    }
    const init = tags.map(enumField).join('\n  ')
    return '(function(){\n  const f = (x,...args) => args[x._index](x.val)\n  ' + init + '\nreturn f })()'
  }
  function gen(node) {
    try {
      if (node.code === '[') {
        return '[' + node.argv.map(gen).join(',') + ']'
      } else if (node.code === '(') {
        return '(' + gen(node.argv[0]) + ')'
      } else if (node.code === ':') {
        const [name,...fields] = node.argv.map(id)
        const ids = fields.join(',')
        return 'const ' + name + ' = ((' + ids + ') => ({' + ids + '}))'
      } else if (node.code === ':|') {
        return 'const ' + id(node.argv[0]) + ' = ' + enumFields(node.argv.slice(1))
      } else if (node.code == '=') {
        return 'const ' + id(node.argv[0]) + ' = ' + gen(node.argv[1])
      } else if (node.code == ':=') {
        return 'let ' + id(node.argv[0]) + ' = new _eff(' + gen(node.argv[1]) + ')'
      } else if (node.op2 && node.code === '=>') {
        return '((' + gen(node.argv[0]) + ') => ' + gen(node.argv[1]) + ')'
      } else if (node.op2 && node.code === '/') {
        return '_div(' + gen(node.argv[0]) + ',' + gen(node.argv[1]) + ')'
      } else if (node.op2) {
        return gen(node.argv[0]) + node.code + gen(node.argv[1])
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
  function _eff(val) {
    this.val = val
  }
  function error(...args) {
    function f(message, ...args) {
      this.message = message.toString()
      this.args = args
      try {
        throw new Error()
      } catch (e) {
        this.id = e.stack
      }
    }
    f.prototype.valueOf = function() {
      return 'error: ' + this.message
    }
    f.prototype.alt = function(v) {
      return v
    }
    f.prototype.catch = function(expect, alt) {
      return expect.id === this.id ? alt : this
    }
    return new f(...args)
  }
  _eff.prototype.eff = function(op, rhs) {
    switch(op) {
      case '+=': return () => this.val += rhs
      case '-=': return () => this.val -= rhs
      case '*=': return () => this.val *= rhs
      case '/=': return () => {
        return this.val = _div(this.val, rhs)
      }
    }
  }
  _eff.prototype.valueOf = function() {
    return this.val
  }
  _eff.prototype.isEff = true

  // WARN: global pollution
  Object.prototype.alt = function() { return this }
  Object.prototype.catch = function() { return this }
  String.prototype.__defineGetter__('int', function() { return parseInt(this) })
  Number.prototype.__defineGetter__('string', function() { return this.toString() })

  // evaluate source in sandbox
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
    title("expect: ", e.expect)
    title("actual: ", e.actual)
    title("source: ", e.source)
    title("stdout: ", e.stdout)
    title("error : ", e.error)
    title("js    : ", e.js)
    title("nodes : ", e.nodes)
    title("tokens: ", e.tokens)
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
    // container
    t.eq([1,2], '[1 2]')
    // exp
    t.eq(5, "2 + 3")
    t.eq(-1, "2 - 3")
    t.eq(6, "2 * 3")
    t.eq(2, "6 / 3")
    t.eq(14, "2 + 3 * 4")
    t.eq(20, "(2 + 3) * 4")
    t.eq(10, "2 * 3 + 4")
    t.eq(14, "2 * (3 + 4)")
    // definition
    t.eq(3, 'a+b', 'a=1', 'b=2')
    // struct
    t.eq({a:1, b:[]}, 'ab(1 [])', 'ab: a int, b []int')
    t.eq([2,3], 'ab(1 [2 3]).b', 'ab: a int, b []int')
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
    t.eq(1, "do(nil).alt(1)", 'nil = error("nil")')
    t.eq(2, "do(2).alt(1)")
    t.eq(2, "b.catch(a 1).catch(b 2)", 'a = error("msg")', 'b = error("msg")')
    // built-in
    t.eq(11, '"11".int')
    t.eq('1,2', '[1 2].map(x => x.string).join(",")')
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
