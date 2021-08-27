const write = (...a) => a.map(o => process.stdout.write(o.toString()))
const warn = (...a) => a.map(o => process.stderr.write(o.toString()))
const print = (...a) => console.log(...a)
const dump = (...a) => { a.slice(0,-1).map(s => write(s, ' ')); console.dir(a.slice(-1),{depth:null}) }
const str = o => JSON.stringify(o, null, '  ')
const eq = (x, y) => str(x) === str(y)
const fail = (message,obj) => { dump(message, obj || {}); throw new Error(message) }
const dict = (ks,vs) => ks.reduce((d,k,i) => (d[k]=vs[i], d), {})
const priorities = [
  '=>',
  '<- := += -= *= /=',
  '|| &&',
  '== != > < >= <=',
  '+ -',
  '* // /',
].map(ops => ops.split(' '))
const priority = op => priorities.findIndex(ops => ops.includes(op))
const newType = (type,indent,o) => Object.assign({type,indent},o)
function struct(d) {
  for (const k in d) {
    this[k] = d[k]
  }
}
const isPrimitive = o => (t =>
  t === 'number' || t === 'string' || t === 'function' ||
  (t === 'object' && (o.constructor === Array || o.constructor === Error || o.constructor === struct))
)(typeof o)

const tokenize = src => {
  let index = 0
  let line = 1
  let indent = 0
  const match = (tag, reg, f) => (m => m && ({tag, index, line, indent, code: m[0], value: f && f(m[0])}))(src.slice(index).match(reg))
  const any = (tag, a) => (code => code && ({tag, index, line, indent, code}))(a.find(v => src.slice(index).startsWith(v)))
  const next = () => match('int', /^[0-9]+/, parseInt) ||
    match('string', /^"[^"]*"+/, s => s.slice(1, -1)) ||
    match('string', /^'[^']*'+/, s => s.slice(1, -1)) ||
    match('string', /^`[^`]*`+/, s => s.slice(1, -1)) ||
    match('id', /^(?:true|false)(?![a-zA-Z0-9_])/, s => s === 'true') ||
    match('id', /^[a-zA-Z_0-9]+/) ||
    any('op2', priorities.flat()) ||
    any('dot', ['.']) ||
    any('sym', '[]()=:|'.split('')) ||
    match('space', /^[ \n]+/) ||
    fail('Failed to tokenize:', {src, index, around: src.slice(index)})
  const tokens = []
  while (index < src.length) {
    const token = next()
    index += token.code.length
    line += (token.code.match(/\n/g) || []).length
    indent = (m => m ? m[0].length - 1 : indent)(token.code.match(/\n *$/))
    tokens.push(token)
  }
  return tokens.filter(t => t.tag !== 'space')
}
const parse = tokens => {
  let pos = 0
  const next = () => tokens[pos++] || fail('EOT', tokens)
  const consume = () => {
    let node = next()
    if (node.code === ')' || node.code === ']') {
      return node
    }
    if (node.code === '(') {
      node = consume()
      guard(t => t.code === ')')
      node.fixed = true // prevent to change priority
    }
    if (node.code === '[') {
      const values = until(t => t.code !== ']', consume, consume)
      node = newType('array', node.indent, {values})
    }
    while (pos < tokens.length && tokens[pos].code === '(' && tokens[pos - 1].index + tokens[pos - 1].code.length === tokens[pos].index) {
      const indent = tokens[pos].indent
      pos+=1 // drop '('
      const argv = until(t => t.code !== ')', consume, consume)
      node = newType('call', indent, {body: node, argv})
    }
    while (pos < tokens.length && tokens[pos].tag === 'dot') {
      pos+=1 // drop '.'
      const name = tokens[pos++].code
      if (pos < tokens.length && tokens[pos].code === '(' && tokens[pos - 1].index + tokens[pos - 1].code.length === tokens[pos].index) {
        const indent = tokens[pos].indent
        pos+=1 // drop '('
        const argv = until(t => t.code !== ')', consume, consume)
        node = newType('method', node.indent, {target: node, name, argv})
      } else {
        node = newType('method', node.indent, {target: node, name, argv: []})
      }
    }
    if (pos < tokens.length && tokens[pos].tag === 'op2') {
      const op2 = consume().code
      const lhs = node
      const rhs = consume()
      if (rhs.op2 && !lhs.fixed && !rhs.fixed && priority(op2) > priority(rhs.op2)) {
        node = newType('op2', node.indent, {
          op2: rhs.op2,
          lhs: newType('op2', node.indent, {op2, lhs, rhs: rhs.lhs}),
          rhs: rhs.rhs})
      } else {
        node = newType('op2', node.indent, {op2, lhs, rhs})
      }
    }
    return node
  }
  const guard = f => (node => f(node) ? node : fail('Unexpected node:' + f.toString(), {node, pos, nodes}))(consume())
  const until = (f, g, h) => {
    const matches = []
    while (pos < tokens.length) {
      const bk = pos
      const node = g()
      if (f(node)) {
        matches.push(node)
      } else {
        pos = bk
        break
      }
    }
    h && h()
    return matches
  }
  const consumeStruct = indent => {
    const fields = until(t => t.indent > indent, consume)
    return [...Array(fields.length/2).keys()].map(i => fields[i*2].code)
  }
  const consumeAdt = () => {
    const types = []
    while (pos < tokens.length && tokens[pos].indent > 0) {
      const {indent, code} = consume()
      const type = newType('case', indent, {id: code, fields: []})
      if (pos < tokens.length && tokens[pos].code === ':') {
        consume() // drop ':'
        type.fields = consumeStruct(2)
      }
      types.push(type)
    }
    return types
  }
  const consumeBody = indent => {
    const lines = until(t => t.indent > indent, top)
    if (lines.length === 0) {
      return consume()
    } else {
      return newType('stmt', lines[0].indent, {lines})
    }
  }
  const top = () => {
    const head = consume()
    const {code, indent, line} = head
    const args = until(t => t.tag === 'id' && t.line === line, consume).map(t => t.code)
    if (pos < tokens.length) {
      switch (tokens[pos].code) {
        case '=': consume(); return newType('func', indent, {id: code, args, body: consumeBody(indent)})
        case ':': consume(); return newType('struct', indent, {id: code, args, struct: consumeStruct(0)})
        case '|': consume(); return newType('adt', indent, {id: code, args, adt: consumeAdt()})
      }
    }
    if (args.length > 0) {
      fail('invalid syntax', {head, args, pos, tokens})
    }
    return head
  }

  const nodes = []
  while (pos < tokens.length) {
    nodes.push(top())
  }
  return nodes
}
const execute = (nodes, opt) => {
  opt ||= {}
  const newSpace = (d, p) => ({
    get(k) { return k in d ? d[k] : p.get(k) },
    put(k, v) { d[k] = v },
    update(k, v) { k in d ? d[k] = v : p.update(k, v) },
    local(d2) { return newSpace(d2, this) }
  })
  const space = newSpace({}, {
    get(k) { throw new Error(k + ' is not found') },
    put(k,v) { throw new Error('never comming here') },
    update(k, v) { throw new Error(k + ' is not found to udpate ' + set(v)) },
  })
  for (const node of nodes) {
    if (node.type === 'func') {
      space.put(node.id, node.args.length === 0 ? node.body : node)
    } else if (node.type === 'struct') {
      space.put(node.id, node)
    } else if (node.type === 'adt') {
      for (const type of node.adt) {
        space.put(type.id, type)
      }
    } else {
      fail('Unknown node', {node, nodes})
    }
    node.defined = true
  }
  const methods = {
    'array': {
      size: a => a.length,
      at: (a,i) => i >= 0 ? a[i] : a[a.length + i],
      map: (a,f) => a.map(f),
    },
    'string': {
      size: s => s.length,
      at: (s,i) => i >= 0 ? s[i] : s[a.length + i],
    },
  }
  const method = (env, target, name, argv) => {
    const t = typeof target
    const table = t === 'object' && (
      target.constructor === Array ? methods.array :
      target.constructor === struct ? target :
      node.type || fail('No type information', {node, target})) || methods[t]
    const m = table[name]
    if (typeof m === 'function') {
      return m(target, ...argv)
    } else if (argv.length === 0) {
      return m
    } else {
      fail('Not method', {m, target, name, argv})
    }
  }
  const run = (env, node) => {
    if (node === undefined) {
      throw new Error('node is undefined in ' + str(env))
    }
    if (isPrimitive(node)) {
      return node
    } else if (node.value !== undefined) {
      return node.value
    } else if (node.type === 'array') {
      return node.values.map(o => run(env, o))
    } else if (node.type === 'method') {
      return method(env, run(env, node.target), node.name, node.argv.map(a => run(env, a)))
    } else if (node.type === 'op2') {
      if (node.op2 === '=>') {
        const args = [node.lhs.code]
        const body = node.rhs
        return (...a) => run(env.local(dict(args, a)), body)
      } else if (node.op2 === '<-') {
        return env.put(node.lhs.code, run(env, node.rhs))
      } else if (':= += -= *= /='.split(' ').includes(node.op2)) {
        const r = run(env, node.rhs)
        const l = env.get(node.lhs.code)
        switch (node.op2) {
          case ':=': return env.update(node.lhs.code, r)
          case '+=': return env.update(node.lhs.code, l + r)
          case '-=': return env.update(node.lhs.code, l - r)
          case '*=': return env.update(node.lhs.code, l * r)
          case '/=': return env.update(node.lhs.code, l / r)
          default: fail('Unknown op2', node)
        }
      }
      const l = run(env, node.lhs)
      const r = run(env, node.rhs)
      switch (node.op2) {
        case '+': return l + r
        case '-': return l - r
        case '*': return l * r
        case '/': return l / r
        case '//': return parseInt(l / r)
        case '==': return eq(l, r)
        case '!=': return !eq(l, r)
        case '>':  return l > r
        case '<':  return l < r
        case '>=': return l >= r
        case '<=': return l <= r
        case '||': return l || r
        case '&&': return l && r
        default: fail('Unknown op2', node)
      }
    } else if (node.type === 'struct') {
      return node
    } else if (node.type === 'case') {
      if (node.fields.length === 0) {
        return {_case: node.id}
      } else {
        return node
      }
    } else if (node.type === 'func') {
      if (!node.defined) {
        env.put(node.id, node.args.length === 0 ? node.body : node)
        node.defined = true
      }
      return (...a) => run(env.local(dict(node.args, a)), node.body)
    } else if (node.type === 'stmt') {
      return node.lines.map(line => run(env, line)).slice(-1)[0]
    } else if (node.type === 'call') {
      if (node.body.code === 'if') {
        for (let i=0; i<node.argv.length; i+=2) {
          if (run(env, node.argv[i]) === true) {
            return run(env, node.argv[i + 1])
          }
        }
        return run(env, node.argv.slice(-1)[0])
      } else if (node.body.code === 'match') {
        const target = run(env, node.argv[0])
        for (let i=1; i<node.argv.length; i+=2) {
          if (target._case === node.argv[i].code) {
            const v = run(env, node.argv[i + 1])
            return typeof v === 'function' ? v(target) : v
          }
        }
        throw new Error('Unmatch' + str(target))
      } else if (node.body.code === 'then') {
        try {
          const target = run(env, node.argv[0])
          const f = run(env, node.argv[1])
          return f(target)
        } catch (e) {
          throw e
        }
      } else if (node.body.code === 'catch') {
        try {
          return run(env, node.argv[0])
        } catch (e) {
          const f = run(env, node.argv[1])
          const t = new struct({message: e.message})
          return f(t)
        }
      } else if (node.body.code === 'error') {
        throw new Error(run(env, node.argv[0]))
      }
      const f = run(env, node.body)
      const argv = node.argv.map(o => run(env, o))
      if (f.type === 'struct') {
        return new struct(dict(f.struct, argv))
      } else if (f.type === 'case') {
        return new struct(Object.assign({_case: f.id}, dict(f.fields, argv)))
      } else if (typeof f === 'function') {
        return f(...argv)
      } else {
        fail('Not callable', {f,argv})
      }
    } else if (node.tag === 'id') {
      return run(env, env.get(node.code))
    }
    fail('Failed to run', {node, nodes})
  }
  let stdout = ''
  const io = {
    argv: (_,i) => (opt.argv && opt.argv[i]) || '',
    read: opt.stdin,
    write: (_,s) => stdout += s.toString(),
  }
  space.put('io', new struct(io))
  space.put('__debug', (...a) => warn('warn:', ...a.map(str), '\n'))
  try {
    const ret = run(space, space.get('main'))
    return {ret, stdout}
  } catch (ret) {
    return {ret, stdout}
  }
}

const testJs = () => {
  const test = (opt, check, expect, exp, ...defs) => {
    const src = (defs || []).concat(['main = ' + exp]).join('\n')
    const tokens = tokenize(src)
    const nodes = parse(tokens)
    const result = execute(nodes, opt)
    if (eq(expect, check(result))) {
      write('.')
    } else {
      print('src   :', src)
      print('expect:', expect)
      print('result:', result)
      dump('tokens :', tokens)
      dump('nodes  :', nodes)
      throw new Error('Test was failed')
    }
  }
  const t = (expect, exp, ...defs) => test({}, x => x.ret, expect, exp, ...defs)
  const f = (expect, exp, ...defs) => test({}, x => x.ret.message, expect, exp, ...defs)
  const stdout = (expect, exp, ...defs) => test({}, x => x.stdout, expect, exp, ...defs)
  const stderr = (expect, exp, ...defs) => test({}, x => x.stderr, expect, exp, ...defs)
  const stdio = (opt, expect, exp, ...defs) => test(opt, x => x.stdout, expect, exp, ...defs)

  // primitives
  t(1, '1')
  t('hi', '"hi"')
  t('hi', "'hi'")
  t('h\ni', '`h\ni`')
  t(false, 'false')
  t(3, '(a => a + 2)(1)')

  // expression
  t(3, '1 + 2')
  t(1, '3 - 2')
  t(6, '2 * 3')
  t(1.5, '3 / 2')
  t(2, '4 // 2')
  t(true, '1 != 2')
  t(false, '1 == 2')
  t(false, '1 > 2')
  t(true, '1 < 2')

  // parenthesis
  t(3, '(1 + 2)')
  t(6, '(1) + ((2 + 3))')
  t(14, '2 * (3 + 4)')

  // precedence of operatos
  t(5, '1 + 2 * 3 - 4 / 2')
  t(true, '1 == 2 || 3 != 4')

  // containers
  t([], '[]')
  t([1], '[1]')
  t([1, 2], '[1 2]')
  t([1, 5], '[1 2+3]')

  // control flow
  t(1, 'if(true 1 main)')
  t(2, 'if(false main 2)')
  t(3, 'if(false main false main 3)')

  // functions
  t(3, 'f + 2', 'f = 1')
  t(3, 'f(1)', 'f a = a + 2')

  // methods
  t(1, '[1].size')
  t(2, '"hi".size')
  t(2, 'id("hi").size', 'id a = a')
  t('i', '"hi".at(1)')
  t([2, 3], '[1 2].map(x => x + 1)')
  t([2, 3], '[1 2].map(inc)', 'inc n = n + 1')
  t(2, '[[1 2]].at(0).at(1)')
  t('hi', '[[s("hi")]].at(0).at(0).name', 's:\n  name string')

  // generics
  t('i', 'id("hi").at(id(1))', 'id a = a')

  // recursive
  t(120, 'f(5)', 'f n = if((n > 1) (n * f(n - 1)) 1)')

  // struct
  t({a: 1}, 's(1)', 's:\n  a int')
  t({a: 1, b: "b"}, 's(1 "b")', 's:\n  a int\n  b string')
  t(3, 's(1 2).a + s(1 2).b', 's:\n  a int\n  b int')

  // adt
  t({_case: 'a'}, 'a', 't|\n  a')
  t({_case: 'a', x: 1}, 'a(1)', 't|\n  a:\n    x int')

  // match
  t(1, 'match(a a 1 b 2)', 't|\n  a\n  b')
  t(2, 'match(b a 1 b 2)', 't|\n  a\n  b')
  t(3, 'match(b(2) a 1 b inc)', 't|\n  a\n  b:\n    num int', 'inc o = o.num + 1')

  // option
  t(3, 'then(1 v => (v + 2))')
  f("err", 'error("err")')
  f("err", 'then(error("err") v => v)')
  t("err", 'catch(error("err") e => e.message)')

  // monadic statement
  t(1, '\n  1')
  t(2, '\n  1\n  2')
  t(5, '\n  a <- f(1)\n  b <- f(a)\n  a + b', 'f v = v + 1')
  f("err", '\n  error("err")\n  2')
  f("err", '\n  error("err") + 1')
  f("err1", '\n  f(error("err1") error("err2"))', 'f a b = b')

  // modify variable
  t(3, '\n  a <- 1\n  a += 2\n  a')
  t(1, '\n  a <- 1\n  a0(a)\n  a', 'a0 a = a := 0')
  t(3, '\n  a <- 1\n  inc =\n    a += 1\n  inc\n  inc\n  a')
  t(6, '\n  a <- 1\n  add n =\n    a += n\n  add(2)\n  add(3)\n  a')

  // io
  stdout('hi', 'io.write("hi")')
  stdout('1', 'io.write(1)')
  stdio({stdin: 'input'}, 'input', 'io.write(io.read)')
  stdio({argv: ['a.moa', 'hi']}, 'hi', 'io.write(io.argv(1))')
}

if (process.argv[2] === 'test') {
  testJs()
  print('ok')
} else {
  const fs = require('fs')
  const src = fs.readFileSync('moa.moa', 'utf-8')
  const target = fs.readFileSync('/dev/stdin', 'utf-8')
  const tokens = tokenize(src)
  const nodes = parse(tokens)
  const result = execute(nodes, {stdin: target, argv: process.argv.slice(1)})
  print(result.stdout.replace(/\\n/g, '\n'))
}
