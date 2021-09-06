const dump = (...a) => console.dir(a.length === 1 ? a[0] : a, {depth: null})
const print = (...a) => console.log(...a)
const write = (...a) => a.map(an => process.stdout.write(an.toString()))
const trace = (...a) => (dump(...a),a[a.length-1])
const str = o => JSON.stringify(o)
const eq = (a,b) => JSON.stringify(a) === JSON.stringify(b)
const isDefine = o => typeof o === 'string' && '=:|'.includes(o)
const isOp = o => typeof o === 'string' && '+-*/.'.includes(o[0])
const isBr = o => typeof o === 'string' && o[0] === '\n'
const isArray = o => typeof o === 'object' && o.constructor === Array
const tokenize = src => [...src.matchAll(/[ \n]+|[()]|[+\-*\/=:|.]+|"[^"]*"|[0-9]+|[a-zA-Z_]+/g)].map(t => t[0].replace(/^ +/, '')).filter(t => t)
const fail = (msg, ...a) => { dump(msg, ...a);  throw new Error(msg) }
const field = (target, name) => newType('field', {target, name})
const parse = tokens => {
  let pos = 0
  const last = (a, i) => a[a.length - i]
  const len = tokens.length
  const next = v => (++pos, v)
  const group = (l, r) => r.length === 0 ? l :
    l.length > 1 && isDefine(last(l, 1)) ? [last(l, 1), l[0], l.slice(1, -1), group([], r)] :
    l.length > 1 && isOp(last(l, 1)) ? [last(l, 1), l[0], unnest(group([], r))] :
    l.length > 1 && isBr(last(l, 1)) ? [unnest(l.slice(0, -1)), group([], r)] :
    l.length === 1 && isBr(l[0]) ? group([], r) :
    group(l.concat(r[0]), r.slice(1))
  const many = (acc, f, g) => pos < len && f(tokens[pos]) ? many(acc.concat([g(tokens[pos])]), f, g) : group([], acc)
  const unit = t => t === ')' ? t :
    t === '(' ? [[next(many([], t => t !== ')', consume))]] :
    t.match(/^[0-9]+$/) ? parseInt(t) :
    t
  const consume = () => pos < len ? unit(tokens[pos++]) : fail('EOF', {pos,len,tokens})
  const unnest = o => isArray(o) && o.length === 1 ? unnest(o[0]) : o
  return many([], () => true, () => next([many([], t => t !== '\n', consume)]))
}
const generate = nodes => {
  const body = node => isArray(node) && isArray(node[0]) && node.length > 1 ? statement(node) : compile(node)
  const compile = node => {
    if (typeof node === 'string') {
      return node
    } else if (typeof node === 'number') {
      return node
    } else if (isArray(node)) {
      const head = node[0]
      if (head === '=') {
        return 'const ' + node[1] + ' = (' + node[2].join(',') + ') => ' + body(node[3])
      } else if (node.length === 1) {
        return compile(head)
      } else {
        fail('Unable to compile to javascript', {head, node})
      }
    } else {
      fail('Unable to compile to javascript', node)
    }
  }
  return nodes.map(compile).join('\n')
}
const inference = nodes => {
  return nodes
}
const run = (src, env) => {
  const tokens = tokenize(src)
  const nodes = parse(tokens)
  const typedNodes = inference(nodes)
  const js = generate(typedNodes)
  const stdout = []
  const io = {
    write: (...a) => stdout.push(a.map(o => o.toString()).join(' ')),
  }
  let ret
  try {
    ret = Function(js + '\nreturn main()')()
  } catch (e) {
    ret = e
  }
  return {
    js: js,
    stdout: stdout.join(''),
    tokens,
    nodes,
    typedNodes,
    ret: ret
  }
}
function testAll() {
  const test = (env, f, expect, exp, ...defs) => {
    const src = defs.concat(['main=' + exp]).join('\n')
    const result = run(src, env)
    if (eq(expect, f(result))) {
      write('.')
    } else {
      print('src: ', src)
      print('expect: ', expect)
      print('actual: ', str(f(result)))
      dump('result: ', result)
      process.exit(1)
    }
  }
  const t = (...a) => test({}, (r => r.ret), ...a)
  const out = (...a) => test({}, (r => r.stdout), ...a)
  t(1, '1')
  t('hi', '"hi"')
  //out('1', 'io.print(1)')
  //t(1, '\n  f 1\n  2',  'f = 1 + 2', 'g a b = (a * b) + (1 / 2)')
  //t(1, 'struct("hello" 38)', 'struct a:\n  name string\n  age a')
  //t(1, 'f1(1)', 'adt a:\n  f1 int\n  f2 a')
}

if (process.argv[2] === 'test') {
  testAll()
  print('ok')
}
