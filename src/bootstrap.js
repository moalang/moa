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
const tokenize = src => [...src.matchAll(/[ \n]+|[()]|[+\-*\/=:|.]+|"[^"]*"|[0-9]+|[a-zA-Z_][a-zA-Z_0-9]*\(?/g)].map(t => t[0].replace(/^ +/, '')).filter(t => t)
const fail = (msg, ...a) => { dump(msg, ...a);  throw new Error(msg) }
const field = (target, name) => newType('field', {target, name})
const parse = tokens => {
  let pos = 0
  const last = (a, i) => a[a.length - i]
  const len = tokens.length
  const next = v => (++pos, v)
  const group = (l, r) => r.length === 0 ? l :
    l.length > 1 && isDefine(last(l, 1)) ? [last(l, 1), l[0], l.slice(1, -1), group([], r)] :
    l.length > 1 && isOp(last(l, 1)) ? [last(l, 1), l[0]].concat(unnest(group([], r))) :
    l.length > 1 && isBr(last(l, 1)) ? [unnest(l.slice(0, -1)), group([], r)] :
    l.length === 1 && isBr(l[0]) ? group([], r) :
    group(l.concat(r[0]), r.slice(1))
  const many = (acc, f, g) => pos < len && f(tokens[pos]) ? many(acc.concat([g(tokens[pos])]), f, g) : group([], acc)
  const unit = t => t === ')' ? t :
    t === '(' ? [[next(many([], t => t !== ')', consume))]] :
    t.endsWith('(') ? [[next(many([t.slice(0, -1)], t => t !== ')', consume))]] :
    t
  const consume = () => pos < len ? unit(tokens[pos++]) : fail('EOF', {pos,len,tokens})
  const unnest = o => isArray(o) && o.length === 1 ? unnest(o[0]) : o
  return many([], () => true, () => next([many([], t => t !== '\n', consume)]))
}
const generate = nodes => {
  const body = node => isArray(node) && isArray(node[0]) && node.length > 1 ? statement(node) : compile(node)
  const compile = node => {
    if (isArray(node)) {
      const head = node[0]
      if (head === '=') {
        return 'const ' + node[1] + ' = (' + node[2].join(',') + ') => ' + body(node[3])
      } else if (head === ':') {
        const field = node[3].map(f => f[0]).join(',')
        return 'const ' + node[1] + ' = (' + field + ') => ({' + field + '})'
      } else if (head === '|') {
        dump(node)
        const adt = ''
        return adt.join('\n') + 'const ' + node[1] + ' = ({' + node[3].map(t => t[0] + ':' + t[0]).join(',') + '})'
      } else if (head === '.') {
        return node[1] + '.' +  node[2] + '(' + node.slice(3).map(compile).join(', ') + ')'
      } else if (node.length === 1) {
        return compile(head)
      } else {
        return compile(head) + '(' + node.slice(1).map(compile).join(', ') + ')'
      }
    } else {
      return node
    }
  }
  return nodes.map(compile).join('\n')
}
const run = (src, env) => {
  const tokens = tokenize(src)
  const nodes = parse(tokens)
  const js = generate(nodes)
  const stdout = []
  global.io = {
    write: (...a) => stdout.push(a.map(o => o.toString()).join(' ')),
    read: () => env.stdin,
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
  out('1', 'io.write 1')
  out('hi 1', 'io.write("hi" 1)')
  t({name: "hello", age: 38}, 'struct("hello" 38)', 'struct a:\n  name string\n  age a')
  //t({_tag: 'f1', f1: 1}, 'f1(1)', 'adt a|\n  f1:\n    value int\n  f2:\n    value a')
}

if (process.argv[2] === 'test') {
  testAll()
  print('ok')
}
