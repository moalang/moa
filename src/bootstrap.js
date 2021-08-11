const write = (...a) => a.map(o => process.stdout.write(o.toString()))
const print = (...a) => console.log(...a)
const dump = (label,o) => { write(label, ' '); console.dir(o,{depth:null}) }
const str = o => JSON.stringify(o, null, '  ')
const eq = (x, y) => str(x) === str(y)
const fail = (message, obj) => { dump(message, obj || {}); throw new Error(message) }
const dict = (ks,vs) => ks.reduce((d,k,i) => (d[k]=vs[i], d), {})

const priorities = [
  '|| &&',
  '== !=',
  '+ -',
  '* / //',
].map(ops => ops.split(' '))
const priority = op => priorities.findIndex(ops => ops.includes(op))

const tokenize = src => {
  let index = 0
  const match = (tag, reg, f) => (m => m && ({tag, index, code: m[0], value: f && f(m[0])}))(src.slice(index).match(reg))
  const any = (tag, a) => (code => code && ({tag, index, code}))(a.find(v => src.slice(index).startsWith(v)))
  const next = () => match('int', /^[0-9]+/, parseInt) ||
    match('string', /^"[^"]*"+/, s => s.slice(1, -1)) ||
    match('id', /^(?:true|false)(?![a-zA-Z0-9_])/, s => s === 'true') ||
    match('id', /^[a-zA-Z_0-9]+/) ||
    any('op2', '|| && == != //'.split(' ').concat('+-*/'.split(''))) ||
    any('sym', '[]()='.split('')) ||
    match('space', /^[ \n]+/) ||
    fail('Failed to tokenize:', {src, index, around: src.slice(index)})
  const tokens = []
  while (index < src.length) {
    const token = next()
    index += token.code.length
    tokens.push(token)
  }
  return tokens.filter(t => t.tag !== 'space')
}
const parse = tokens => {
  let pos = 0
  const newType = (type,o) => Object.assign({type}, o)
  const next = () => tokens[pos++] || fail('EOT', tokens)
  const consume = () => {
    let node = next()
    if (node.code === ')') {
      return node
    }
    if (node.code === '(') {
      node = consume()
      guard(t => t.code === ')')
      node.fixed = true
    }
    if (pos < tokens.length && tokens[pos].tag === 'op2') {
      const op2 = consume().code
      const lhs = node
      const rhs = consume()
      if (rhs.op2 && !lhs.fixed && !rhs.fixed && priority(op2) > priority(rhs.op2)) {
        node = {op2: rhs.op2, lhs: {op2, lhs, rhs: rhs.lhs}, rhs: rhs.rhs}
      } else {
        node = {op2, lhs, rhs}
      }
    }
    if (pos < tokens.length && tokens[pos].code === '(' && tokens[pos - 1].index + tokens[pos - 1].code.length === tokens[pos].index) {
      pos += 1 // drop "("
      const argv = until(t => t.code !== ')', consume)
      node = newType('call', {argv, body: node})
    }
    return node
  }
  const guard = f => (node => f(node) ? node : fail('Unexpected node:' + f.toString(), node))(consume())
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
  const exp = () => {
    const node = consume()
    if (node.code === '[') {
      const values = until(t => t.code !== ']', consume)
      return newType('array', {values})
    }
    return node
  }
  const top = () => {
    const id = consume().code
    const args = until(t => t.tag === 'id').map(t => t.code)
    guard(t => t.code === '=')
    const body = exp()
    return newType('func', {id, args, body})
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
    }
  }
  const run = node => {
    if (node.value !== undefined) {
      return node.value
    }
    if (node.values) {
      return node.value = node.values.map(run)
    }
    if (node.op2) {
      switch (node.op2) {
        case '+': return node.value = run(node.lhs) + run(node.rhs)
        case '-': return node.value = run(node.lhs) - run(node.rhs)
        case '*': return node.value = run(node.lhs) * run(node.rhs)
        case '/': return node.value = run(node.lhs) / run(node.rhs)
        case '//': return node.value = parseInt(run(node.lhs) / run(node.rhs))
        case '==': return node.value = eq(run(node.lhs), run(node.rhs))
        case '!=': return node.value = !eq(run(node.lhs), run(node.rhs))
        case '||': return node.value = run(node.lhs) || run(node.rhs)
        case '&&': return node.value = run(node.lhs) && run(node.rhs)
        default: fail('Unknown op2', node)
      }
    }
    if (node.type === 'func') {
      if (node.args.length === 0) {
        return node.value = run(node.body)
      } else {
        return node
      }
    }
    if (node.type === 'call') {
      if (node.body.code === 'if') {
        for (let i=0; i<node.argv.length; i+=2) {
          if (run(node.argv[i]) === true) {
            return run(node.argv[i + 1])
          }
        }
        return run(node.argv.slice(-1)[0])
      }
      const f = run(scope[node.body.code])
      if (f.type === 'func') {
        const d = dict(f.args, node.argv)
        Object.assign(scope, d) // TODO: to use local scope
        return run(f.body)
      } else {
        return f
      }
    }
    if (node.tag === 'id') {
      return run(scope[node.code])
    }
    fail('Failed to run', {node, nodes})
  }
  return run(scope.main.body)
}

const testJs = () => {
  const t = (expect, exp, ...defs) => {
    const src = (defs || []).concat(['main = ' + exp]).join('\n')
    const tokens = tokenize(src)
    const nodes = parse(tokens)
    const result = execute(nodes)
    if (eq(expect, result)) {
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

  // primitives
  t(1, '1')
  t('hi', '"hi"')
  t(false, 'false')

  // expression
  t(3, '1 + 2')
  t(1, '3 - 2')
  t(6, '2 * 3')
  t(1.5, '3 / 2')
  t(2, '4 // 2')
  t(true, '1 != 2')
  t(false, '1 == 2')

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
/*
  // functions
  t(3, 'f = 1\nmain = f + 2')
  t(3, 'f a = a + 2\nmain = f 1')

  // methods
  t(-1, 'main = 1.neg')
  t(1, 'main = [1].size')

  // generics
  t('i', 'id a = a\nmain = id("hi").at(id(1))')

  // recursive
  t(120, 'f n = if (n > 1) (n * (f n - 1)) 1\nmain = f(5)')

  // TODO: implicit curring
  //t(7, 'add a b = a + b\ninc a = add 1\nmain = (inc 1) + (add 2 3)')

  // option
  t(1, 'main = some(1)')
  t(3, 'main = (some 1).map(v => (v + 2))')
  t(1, 'main = (error "hi").alt(1)')

  // monadic statement
  t(1, 'main =\n  some(1)')
  t(2, 'main =\n  some(1)\n  some(2)')
  t({message: 'hi', __type: 'error'}, 'main =\n  error("hi")\n  some(2)')
  */
}

testJs()
print('ok')
