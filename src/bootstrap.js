const write = (...a) => a.map(o => process.stdout.write(o.toString()))
const print = (...a) => console.log(...a)
const dump = (label,o) => { write(label, ' '); console.dir(o,{depth:null}) }
const str = o => JSON.stringify(o, null, '  ')
const eq = (x, y) => str(x) === str(y)
const fail = (message, obj) => { dump(message, obj || {}); throw new Error(message) }
const dict = (ks,vs) => ks.reduce((d,k,i) => (d[k]=vs[i], d), {})
const priorities = [
  ':= += -= *= /=',
  '|| &&',
  '== != > < >= <=',
  '+ -',
  '* // /',
  '=>',
  '.',
].map(ops => ops.split(' '))
const priority = op => priorities.findIndex(ops => ops.includes(op))
const newType = (type,indent,o) => Object.assign({type,indent},o)
const isPrimitive = o => (t =>
  t === 'number' ||
  t === 'string' ||
  t === 'object' && (o.constructor === Array || o.constructor == Error)
)(typeof o)

const tokenize = src => {
  let index = 0
  let line = 1
  let indent = 0
  const match = (tag, reg, f) => (m => m && ({tag, index, line, indent, code: m[0], value: f && f(m[0])}))(src.slice(index).match(reg))
  const any = (tag, a) => (code => code && ({tag, index, line, indent, code}))(a.find(v => src.slice(index).startsWith(v)))
  const next = () => match('int', /^[0-9]+/, parseInt) ||
    match('string', /^"[^"]*"+/, s => s.slice(1, -1)) ||
    match('id', /^(?:true|false)(?![a-zA-Z0-9_])/, s => s === 'true') ||
    match('id', /^[a-zA-Z_0-9]+/) ||
    any('op2', priorities.flat()) ||
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
      const values = until(t => t.code !== ']', consume)
      node = newType('array', node.indent, {values})
    }
    while (pos < tokens.length && tokens[pos].code === '(' && tokens[pos - 1].index + tokens[pos - 1].code.length === tokens[pos].index) {
      const indent = tokens[pos].indent
      pos+=1 // drop '('
      const argv = until(t => t.code !== ')', consume)
      node = newType('call', indent, {body: node, argv})
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
  const until = (f, g) => {
    const matches = []
    while (pos < tokens.length) {
      const bk = pos
      const node = consume()
      if (f(node)) {
        matches.push(node)
      } else {
        pos = bk
        break
      }
    }
    g && g()
    return matches
  }
  const consumeFields = (indent) => {
    const fields = until(t => t.indent > indent)
    return [...Array(fields.length/2).keys()].map(i => fields[i*2].code)
  }
  const consumeAdtFields = () => {
    const types = []
    while (pos < tokens.length && tokens[pos].indent > 0) {
      const {indent, code} = consume()
      const type = newType('tag', indent, {id: code, fields: []})
      if (pos < tokens.length && tokens[pos].code === ':') {
        consume() // drop ':'
        type.fields = consumeFields(2)
      }
      types.push(type)
    }
    return types
  }
  const consumeFunctionBody = (indent) => {
    const lines = until(t => t.indent > indent)
    if (lines.length === 0) {
      return consume()
    } else if (lines.length === 1) {
      return lines[0]
    } else {
      return newType('stmt', lines[0].indent, {lines})
    }
  }
  const top = () => {
    const head = consume()
    const {code, indent} = head
    const args = until(t => t.tag === 'id' && t.indent === indent).map(t => t.code)
    if (pos < tokens.length) {
      switch (tokens[pos].code) {
        case '=': consume(); return newType('func', indent, {id: code, args, body: consumeFunctionBody(indent)})
        case ':': consume(); return newType('struct', indent, {id: code, args, struct: consumeFields(0)})
        case '|': consume(); return newType('adt', indent, {id: code, args, adt: consumeAdtFields()})
      }
    }
    if (args.length === 0) {
      return head
    } else {
      fail('invalid syntax', {pos, tokens})
    }
  }

  const nodes = []
  while (pos < tokens.length) {
    nodes.push(top())
  }
  return nodes
}
const execute = nodes => {
  const scope = {}
  for (const node of nodes) {
    if (node.type === 'func') {
      scope[node.id] = node
    } else if (node.type === 'struct') {
      scope[node.id] = node
    } else if (node.type === 'adt') {
      for (const type of node.adt) {
        scope[type.id] = type
      }
    } else {
      fail('Unknown node', {node, nodes})
    }
  }
  const method = (env, o, node) => {
    if (node.tag === 'id') {
      const id = node.code
      if (typeof o === 'object' && o.constructor === Array) {
        if (id === 'size') {
          return o.length
        }
      }
      if (typeof o === 'object' && o.constructor === Error) {
        if (id === 'message') {
          return o.message
        }
      }
      if (typeof o === 'string') {
        if (id === 'size') {
          return o.length
        }
      }
    } else if (node.type === 'call') {
      const id = node.body.code
      const argv = node.argv
      if (typeof o === 'object' && o.constructor === Array) {
        if (id === 'at') {
          return o[run(env, argv[0])]
        }
      }
      if (typeof o === 'string') {
        if (id === 'at') {
          return o[run(env, argv[0])]
        }
      }
    }
    fail('Unknown method', {o,node})
  }
  const run = (env, node) => {
    if (node === undefined) {
      return node
    }
    if (isPrimitive(node)) {
      return node
    }
    if (node.value !== undefined) {
      return node.value
    } else if (node.values) {
      return node.values.map(o => run(env, o))
    } else if (node.type === 'op2') {
      if (node.op2 === '.') {
        return method(env, run(env, node.lhs), node.rhs)
      } else if (node.op2 === '=>') {
        const args = [node.lhs.code]
        const body = node.rhs
        return newType('func', node.indent, {id: '', args, body})
      } else if (':= += -= *= /='.split(' ').includes(node.op2)) {
        const r = run(env, node.rhs)
        switch (node.op2) {
          case ':=': return env[node.lhs.code] = r
          case '+=': return env[node.lhs.code] += r
          case '-=': return env[node.lhs.code] -= r
          case '*=': return env[node.lhs.code] *= r
          case '/=': return env[node.lhs.code] /= r
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
    } else if (node.type === 'tag') {
      if (node.fields.length === 0) {
        return {_type: node.id}
      } else {
        return node
      }
    } else if (node.type === 'func') {
      if (node.args.length === 0) {
        return run(env, node.body)
      } else {
        return node
      }
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
          if (target._type === node.argv[i].code) {
            return run(env, node.argv[i + 1])
          }
        }
        throw new Error('Unmatch' + str(target))
      } else if (node.body.code === 'then') {
        try {
          const target = run(env, node.argv[0])
          const f = run(env, node.argv[1])
          return run(Object.assign({}, env, dict(f.args, [target])), f.body)
        } catch (e) {
          throw e
        }
      } else if (node.body.code === 'catch') {
        try {
          return run(env, node.argv[0])
        } catch (e) {
          const f = run(env, node.argv[1])
          return run(Object.assign({}, env, dict(f.args, [e])), f.body)
        }
      } else if (node.body.code === 'error') {
        throw new Error(run(env, node.argv[0]))
      }
      const f = run(env, node.body)
      const argv = node.argv.map(o => run(env, o))
      if (f.type === 'func') {
        return run(Object.assign({}, env, dict(f.args, argv)), f.body)
      } else if (f.type === 'struct') {
        return dict(f.struct, argv)
      } else if (f.type === 'tag') {
        return Object.assign({_type: f.id}, dict(f.fields, argv))
      } else {
        return f
      }
    } else if (node.tag === 'id') {
      return run(env, env[node.code])
    }
    fail('Failed to run', {node, nodes})
  }
  try {
    return run(scope, scope.main.body)
  } catch (e) {
    return e
  }
}

const testJs = () => {
  const test = (check, expect, exp, ...defs) => {
    const src = (defs || []).concat(['main = ' + exp]).join('\n')
    const tokens = tokenize(src)
    const nodes = parse(tokens)
    const result = execute(nodes)
    if (eq(expect, check(result))) {
      write('.')
    } else {
      print('expect:', expect)
      print('result:', result)
      dump('tokens :', tokens)
      dump('nodes  :', nodes)
      print('src   :', src)
      throw new Error('Test was failed')
    }
  }
  const t = (expect, exp, ...defs) => test(x => x, expect, exp, ...defs)
  const f = (expect, exp, ...defs) => test(x => x.message, expect, exp, ...defs)

  // primitives
  t(1, '1')
  t('hi', '"hi"')
  t(false, 'false')
  t(1, '(a => a)(1)')

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

  // generics
  t('i', 'id("hi").at(id(1))', 'id a = a')

  // recursive
  t(120, 'f(5)', 'f n = if((n > 1) (n * f(n - 1)) 1)')

  // struct
  t({a: 1}, 's(1)', 's:\n  a int')
  t({a: 1, b: "b"}, 's(1 "b")', 's:\n  a int\n  b string')

  // adt
  t({_type: 'a'}, 'a', 't|\n  a')
  t({_type: 'a', x: 1}, 'a(1)', 't|\n  a:\n    x int')

  // match
  t(1, 'match(a a 1 b 2)', 't|\n  a\n  b')
  t(2, 'match(b a 1 b 2)', 't|\n  a\n  b')

  // option
  t(3, 'then(1 v => (v + 2))')
  f("err", 'error("err")')
  f("err", 'then(error("err") v => v)')
  t("err", 'catch(error("err") e => e.message)')

  // monadic statement
  t(1, '\n  1')
  t(2, '\n  1\n  2')
  f("err", '\n  error("err")\n  2')
  f("err", '\n  error("err") + 1')
  f("err1", '\n  f(error("err1") error("err2"))', 'f a b = b')

  // modify variable
  t(3, '\n  a := 1\n  a += 2\n  a')
}

testJs()
print('ok')
