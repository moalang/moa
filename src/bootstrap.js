const dump = (...a) => console.dir(a.length === 1 ? a[0] : a, {depth: null})
const print = (...a) => console.log(...a)
const write = (...a) => a.map(an => process.stdout.write(an.toString()))
const trace = (...a) => (dump(...a),a[a.length-1])
const eq = (a,b) => JSON.stringify(a) === JSON.stringify(b)
const isDefine = o => typeof o === 'string' && '=:|'.includes(o)
const isOp = o => typeof o === 'string' && '+-*/'.includes(o[0])
const isBr = o => typeof o === 'string' && o[0] === '\n'
const isArray = o => typeof o === 'object' && o.constructor === Array
const tokenize = src => [...src.matchAll(/[ \n]+|[()]|[+\-*\/=]+|[0-9]+|[a-zA-Z_]+/g)].map(t => t[0].replace(/^ +/, '')).filter(t => t)
const fail = (msg, ...a) => { dump(msg, ...a);  throw new Error(msg) }
const parse = tokens => {
  // group by symbols from "f a b = a + b" to "= f [a b] [+ a b]"
  // group by lines based on indent from "f = \n  v <- g 1\n  v" to "= f [[<- v [g 1]] h]"
  let pos = 0
  const last = (a, i) => a[a.length - i]
  const len = tokens.length
  const next = v => (++pos, v)
  const group = (l, r) => r.length === 0 ? l :
    l.length > 1 && isDefine(last(l, 1)) ? [[last(l, 1), l[0], l.slice(1, -1), group([], r)]] :
    l.length > 1 && isOp(last(l, 1)) ? [last(l, 1), l[0], unnest(group([], r))] :
    l.length == 1 && isBr(last(l, 1)) ? group([], r) :
    l.length > 1 && isBr(last(l, 1)) ? [unnest(l.slice(0, -1)), group([], r)] :
    group(l.concat(r[0]), r.slice(1))
  const many = (acc, f, g) => pos < len && f(tokens[pos]) ? many(acc.concat(g(tokens[pos])), f, g) : group([], acc)
  const unit = t => t === ')' ? t :
    t === '(' ? [[next(many([], t => t !== ')', consume))]] :
    t
  const consume = () => pos < len ? unit(tokens[pos++]) : fail('EOF', {pos,len,tokens})
  const unnest = o => isArray(o) && o.length === 1 ? unnest(o[0]) : o
  return many([], () => true, () => next([many([], t => t !== '\n', consume)]))
}
const run = (src, env) => {
  const tokens = tokenize(src)
  const nodes = parse(tokens)
  dump(src, nodes)
  return {
    ret: 1
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
      print('actual: ', f(result))
      print('result: ', result)
      process.exit(1)
    }
  }
  const t = (...a) => test({}, (r => r.ret), ...a)
  t(1, '\n  f 1\n  2',  'f = 1 + 2', 'g a b = (a * b) + (1 / 2)')
}

if (process.argv[2] === 'test') {
  testAll()
  print('ok')
}
