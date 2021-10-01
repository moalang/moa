const print = (...a) => console.log(...a)
const dump = o => console.dir(o,{depth:null})
const str = o => JSON.stringify(o)
const eq = (x,y) => str(x) === str(y)
const fork = (target,branches) => branches.find((cond,body) => cond==='_' || eq(target,cond))[1]
const dict = (kx,vx) => kx.reduce((d,k,i) => (d[k]=vx[i],d), {})
const fail = (msg,o) => {
  dump(o)
  throw new Error(msg)
}
const copy = o => typeof o === 'object' ? JSON.parse(JSON.stringify(o)) : o

const tokenize = src => src.match(/[^ ()=+]+|./g).filter(x=>x.trim().length)
const parse = src => {
  const primitive = value => ({value})
  const list = list => ({list})
  const tokens = tokenize(src)
  let pos = 0
  const consume = f => f(tokens[pos++] || fail('EOF', {src,pos,tokens}))
  const apply = acc => (t => t.value === ')' ? list(acc) : apply(acc.concat(t)))(unit())
  const unit = () => consume(t => t === '(' ? apply([]) : primitive(t))
  const nodes = []
  while (pos < tokens.length) {
    nodes.push(unit())
  }
  return nodes
}
const infer = (nodes,src) => {
  let tvarId = 0
  const tvar = () => (name => ({name,types:[],var:true,instance:null}))((tvarId++).toString())
  const tlambda = (...types) => ({name:'',types:types})
  const ttype = (name) => ({name,types:[]})
  const tint = ttype('int')
  const tbool = ttype('bool')
  const tnil = ttype('nil')
  const fresh = (type, nonGeneric) => {
    const d = {}
    const rec = t => {
      const p = prune(t)
      return p.var ?
        (nonGeneric.includes(p.name) ? p : d[p.name]||=tvar()) :
        ({name:p.name, types:p.types.map(rec)})
    }
    return rec(type)
  }
  const unify = (a,b) => {
    a = prune(a)
    b = prune(b)
    if (a.var) {
      a.instance = b
    } else if (b.var) {
      unify(b, a)
    } else if (!a.var && !b.var) {
      if (a.name !== b.name) { fail('type name miss match', {a,b}) }
      if (a.types.length !== b.types.length) { fail('types miss match', {a,b}) }
      a.types.map((t,i) => unify(t, b.types[i]))
    } else {
      fail('unify', {a,b})
    }
  }
  let env = {
    'true': tbool,
    'false': tbool,
    '+': tlambda(tint, tlambda(tint, tint)),
    '<': tlambda(tint, tlambda(tint, tbool)),
  }
  const local = (node, d, nonGeneric) => {
    const bk = copy(env)
    Object.keys(d).map(k => env[k]=d[k])
    const ret = analyse(node, nonGeneric)
    env = bk
    return ret
  }
  const prune = t => t.var && t.instance ? t.instance = prune(t.instance) : t
  const curry = (argt,body) => argt.length === 1 ? tlambda(argt[0], body) : tlambda(argt[0], curry(argt.slice(1), body))
  const analyse = (node, nonGeneric) => node.type = _analyse(node, nonGeneric)
  const _analyse = (node, nonGeneric) => {
    if (node.list) {
      if (node.list.length === 0) { return tnil }
      let [head,...tail] = node.list
      if (head.value === '=') {
        if (tail.length < 2) { fail('Not enoug argument', {tail,node}) }
        const name = tail[0].value
        if (tail.length === 2) {
          return env[name] = analyse(tail[1], nonGeneric)
        } else {
          const args = tail.slice(1,-1)
          const argt = args.map(_ => tvar())
          const body = tail.slice(-1)[0]
          const d = dict(args.map(a => a.value), argt)
          const ft = curry(argt, local(body, d, nonGeneric.concat(argt.map(t=>t.name))))
          env[name] = ft
          return ft
        }
      }
      if (tail.length) {
        const rt = tvar()
        const ft = analyse(head, nonGeneric)
        unify(curry(tail.map(t => analyse(t, nonGeneric)), rt), ft)
        return rt
      } else {
        return analyse(head, nonGeneric)
      }
    }

    const v = node.value
    if (v) {
      if (v.match(/^[0-9]+/)) {
        return tint
      } else if (env[v]) {
        return fresh(env[v], nonGeneric) || fail(`Not found ${v}`, node)
      } else {
        fail("unkown", {v,src,node,env})
      }
    } else {
      fail("unkown", {src,node,env})
    }
  }
  const show = t => {
    if (t.instance) {
      return show(t.instance)
    } else if (t.name) {
      return t.name
    } else if (t.types && t.types.length) {
      return t.types.map(show).join(' ')
    } else {
      fail("show",t)
    }
  }
  const types = nodes.map(node => analyse(node, []))
  return show(types.slice(-1)[0])
}

const test = (src,expect) => {
  const nodes = parse(src)
  const actual = infer(nodes, src)
  if (eq(actual,expect)) {
    process.stdout.write('.')
  } else {
    console.log('Failed')
    console.log('expect:',expect)
    console.log('actual:',actual)
    console.log('   src:',src)
    console.log(' nodes:', nodes)
  }
}
// primitives
test('1', 'int')
test('true', 'bool')
test('false', 'bool')

// embedded
test('(+ 1 1)', 'int')
test('(< 1 1)', 'bool')

// call
test('(= f 1)(f)', 'int')
test('(= f a a)(f 1)', 'int')

// generics
test('(= f a a)', '0 0')
test('(= f a b b)', '0 1 1')
test('(= f a a)(f 1)(f true)', 'bool')

// combinations
test('(= f x (+ x 1))(= g x (+ x 2))(+ (f 1) (g 1))', 'int')

print('ok')
