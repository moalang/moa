function write(...a) { process.stdout.write(a.map(o => o.toString()).join(' ')) }
function print(...a) { console.log(...a) }
function dump(...a) { a.map(o => console.dir(o, {depth: null})) }
function str(o) { return JSON.stringify(o) }
function err(message, o) { dump(o); return new Error(message) }
function tokenize(src) {
  let line = 1
  let indent = 0
  return [...src.matchAll('[()\\[\\]:+\\-*/.|]|[ \n]+|[^ ()\\[\\]\\n :+\\-*/.|]+')].map(a => {
    const br = (a[0].match(/\n/g) || []).length
    line += br
    if (br) { indent = a[0].split('\n').slice(-1).length }
    return {token: a[0], index: a.index, line, indent}
  }).filter(x => x.token.replace(/[ \n]+/, ''))
}
function parse(tokens) {
  let pos = 0
  function consume() { return pos < tokens.length ? tokens[pos++] : null }
  function until(token) {
    const matches = []
    while (true) {
      const node = unit()
      if (node.token === token) {
        return matches
      } else {
        matches.push(node)
      }
    }
  }
  function unit() {
    const node = consume()
    const token = node.token
    if (token === '[') {
      node.list = until(']')
    } else if (token === '(') {
      node.apply = until(')')
    } else if (['true', 'false'].includes(token)) {
      node.value = token === 'true'
    } else if (parseInt(token).toString() === token) {
      node.value = parseInt(token)
    } else if ('"\'`'.includes(token[0])) {
      node.value = token.slice(1, -1)
    }
    return node
  }
  function align(nodes) {
    nodes = nodes.map(x => x.apply ? align(x.apply) : x)
    const defIndex = nodes.findIndex(x => x.token === '=')
    if (defIndex >= 1) {
      let name = nodes[0].token
      let args = nodes.slice(1, defIndex).map(t => t.token)
      const body = align(nodes.slice(defIndex + 1))
      if (args.length && args[0] === '.') {
        name += '__' + args[1]
        args = args.slice(2)
      }
      return {name, args, body}
    }
    if (nodes.length === 1) {
      return nodes[0]
    } else if (nodes.length >= 3) {
      const l = nodes[0]
      const op = nodes[1].token
      if (op === '.') {
        return {method: [l, ...nodes.slice(2)]}
      } else {
        const r = align(nodes.slice(2))
        if (r.op && '*/'.includes(op) && '+-'.includes(r.op)) {
          return {op: r.op, l: {op, l, r: r.l}, r: r.r}
        } else {
          return {op, l, r}
        }
      }
    } else {
      return {apply: nodes}
    }
  }
  function nest(indent) {
    const all = []
    while (pos < tokens.length && tokens[pos].indent > indent) {
      const line = tokens[pos].line
      const nodes = tokens.slice(pos).filter(x => x.line === line)
      all.push(nodes)
      pos += nodes.length
    }
    return all.map(f => f.map(o => o.token))
  }
  const defs = []
  let stack = []
  let line = 1
  while (pos < tokens.length) {
    const node = unit()
    if (node.token === ':')  {
      const fields = nest(node.indent)
      const name = stack[0].token
      const args = stack.slice(1).map(n => n.token)
      defs.push({name, args, fields})
      stack = []
    } else if (node.token === '|')  {
      const enums = nest(node.indent)
      const name = stack[0].token
      const args = stack.slice(1).map(n => n.token)
      defs.push({name, args, enums})
      stack = []
    } else if (node.line !== line) {
      if (stack.length) {
        defs.push(align(stack))
        line = node.line
      }
      stack = [node]
    } else {
      stack.push(node)
    }
  }
  if (stack.length) {
    defs.push(align(stack))
  }
  return defs
}
function exec(nodes) {
  const env = {}
  for (const node of nodes) {
    env[node.name] = node
  }
  function op2(op, l, r) {
    l = run(l)
    r = run(r)
    switch (op) {
      case '+': return l + r
      case '-': return l - r
      case '*': return l * r
      case '/': return l / r
      default: throw err('unknown op', {op,l,r})
    }
  }
  function run(node) {
    if (node.op) {
      return op2(node.op, node.l, node.r)
    } else {
      return node.value || run(node.body)
    }
  }
  return run(env.main)
}
function testParse() {
  function toValue(node) {
    return node.list ? node.list.map(toValue) : node.value
  }
  function toLisp(node) {
    if (node.apply) {
      if (node.apply.length === 1) {
        return toLisp(node.apply[0])
      } else {
        return '(' + node.apply.map(toLisp).join(' ') + ')'
      }
    } else if (node.list) {
      return '[' + node.list.map(toLisp).join(' ') + ']'
    } else if (node.body) {
      return '(= ' + [node.name, ...node.args, toLisp(node.body)].join(' ') + ')'
    } else if (node.op) {
      return '(' + node.op + ' ' + toLisp(node.l) + ' ' + toLisp(node.r) + ')'
    } else if (node.method) {
      return '(. ' + node.method.map(toLisp).join(' ') + ')'
    } else if (node.fields) {
      return '(: ' + [node.name, ...node.args, ...node.fields.map(f => '(' + f.join(' ') + ')')].join(' ') + ')'
    } else if (node.enums) {
      return '(| ' + [node.name, ...node.args, ...node.enums.map(f => '(' + f.join(' ') + ')')].join(' ') + ')'
    } else {
      return node.token
    }
  }
  function test(expect, src, f) {
    const tokens = tokenize(src)
    const nodes = parse(tokens)
    const actual = f(nodes)
    if (str(expect) === str(actual)) {
      write('.')
    } else {
      print('Failed', src)
      print('expect:', expect)
      print('actual:', actual)
      write('tokens: '); dump(tokens)
      write('nodes : '); dump(nodes)
    }
  }
  function value(expect, src) {
    return test(expect, src, nodes => toValue(nodes[0]))
  }
  function parser(expect, src) {
    return test(expect, src, nodes => nodes.map(toLisp).join('\n'))
  }
  function eq(expect, exp, ...funcs) {
    funcs.push('main = ' + exp)
    return test(expect, funcs.join('\n'), nodes => exec(nodes))
  }

  value(1, '1')
  value('hi', '"hi"')
  value(true, 'true')
  value([], '[]')
  value([1], '[1]')
  value([1, 2], '[1 2]')
  value([1, [true, false]], '[1 [true false]]')
  parser('(+ 1 2)', '1 + 2')
  parser('(+ 1 (* 2 3))', '1 + 2 * 3')
  parser('(+ (* 1 2) 3)', '1 * 2 + 3')
  parser('(=> x x)', 'x => x')
  parser('(=> x (=> y (+ x y)))', 'x => y => x + y')
  parser('(= f 1)', 'f = 1')
  parser('(= f a a)', 'f a = a')
  parser('(= f a b b)', 'f a b = b')
  parser('(= f 1)\n(= g 2)', 'f = 1\ng = 2')
  parser('(= f a (+ a 1))\n(= g (f (f 1)))', 'f a = a + 1\ng = f (f 1)')
  parser('(. 1 abs)', '1.abs')
  parser('(. "hi" size)', '"hi".size')
  parser('(. (+ 1 2) abs)', '(1 + 2).abs')
  parser('(. 1 pow 2)', '1.pow 2')
  parser('(. 1 pow 2 3)', '1.pow 2 3')
  parser('(= int__double n (* n 2))\n(. 1 double)', 'int.double n = n * 2\n1.double')
  parser('(: v2 (x int) (y int))', 'v2:\n  x int\n  y int')
  parser('(: two a b (f1 a) (f2 b))', 'two a b:\n  f1 a\n  f2 b')
  parser('(| b (t) (f))', 'b|\n  t \n  f')
  parser('(| m a (j a) (n))', 'm a|\n  j a \n  n')
  eq(1, '1')
  eq(3, '1 + 2')
  eq(7, '1 + 2 * 3')
  eq(5, '1 * 2 + 3')
}
function main() {
  testParse()
  print('ok')
}
main()
