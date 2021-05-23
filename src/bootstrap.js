function write(...a) { process.stdout.write(a.map(o => o.toString()).join(' ')) }
function print(...a) { console.log(...a) }
function dump(...a) { a.map(o => console.dir(o, {depth: null})) }
function str(o) { return JSON.stringify(o) }
function err(message, o) { dump(o); return new Error(message) }
function tokenize(src) {
  let line = 1
  let indent = 0
  return [...src.matchAll('[()\\[\\]]|[ \n]+|[^ ()\\[\\]\\n ]+')].map(a => {
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
    for (let i=0; i<nodes.length; i++) {
      const node = nodes[i]
      if (node.token === '=') {
        const name = nodes[0].token
        const args = nodes.slice(1, i).map(t => t.token)
        const body = align(nodes.slice(i+1))
        return {name, args, body}
      }
    }
    if (nodes.length === 1) {
      return nodes[0]
    } else if (nodes.length >= 3) {
      const l = nodes[0]
      const op = nodes[1].token
      const r = align(nodes.slice(2))
      if (r.op && '*/'.includes(op) && '+-'.includes(r.op)) {
        return {op: r.op, l: {op, l, r: r.l}, r: r.r}
      } else {
        return {op, l, r}
      }
    } else {
      throw err('nodes', {nodes})
    }
  }
  const defs = []
  let stack = []
  let line = 1
  while (pos < tokens.length) {
    const node = unit()
    if (node.line !== line) {
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
function testParse() {
  function toValue(node) {
    return node.list ? node.list.map(toValue) : node.value
  }
  function toLisp(node) {
    if (node.apply) {
      return '(' + node.apply.map(toLisp).join(' ') + ')'
    } else if (node.list) {
      return '[' + node.list.map(toLisp).join(' ') + ']'
    } else if (node.body) {
      return '(= ' + [node.name].concat(node.args).join(' ') + ' (' + toLisp(node.body) + '))'
    } else if (node.op) {
      return '(' + node.op + ' ' + toLisp(node.l) + ' ' + toLisp(node.r) + ')'
    } else {
      return node.token
    }
  }
  function eq(expect, src, f) {
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
    return eq(expect, src, nodes => toValue(nodes[0]))
  }
  function parser(expect, src) {
    return eq(expect, src, nodes => nodes.map(toLisp).join('\n'))
  }

  value(1, '1')
  value('hi', '"hi"')
  value(true, 'true')
  value([], '[]')
  value([1], '[1]')
  value([1, 2], '[1 2]')
  value([1, [true, false]], '[1 [true false]]')
  parser('(= f (1))', 'f = 1')
  parser('(= f a (a))', 'f a = a')
  parser('(= f a b (b))', 'f a b = b')
  parser('(= f (1))\n(= g (2))', 'f = 1\ng = 2')
  parser('(+ 1 2)', '1 + 2')
  parser('(+ 1 (* 2 3))', '1 + 2 * 3')
  parser('(+ (* 1 2) 3)', '1 * 2 + 3')
}
function main() {
  testParse()
  print('ok')
}
main()
