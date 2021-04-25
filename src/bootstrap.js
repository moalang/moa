// Thanks
// https://github.com/reki2000/hyndley-milner-kotlin/blob/master/HindleyMilner.kt
// http://www.fos.kuis.kyoto-u.ac.jp/~igarashi/class/isle4-10w/testcases.html

const print = (...a) => console.log(...a)
const dump = o => console.dir(o,{depth:null})
const str = o => JSON.stringify(o)
const eq = (x, y) => str(x) === str(y)
const dict = (kx, vx) => kx.reduce((d,k,i) => (d[k]=vx[i],d), {})
const fail = (msg, o) => { throw new Error(msg + ' ' + str(o)) }
const copy = o => JSON.parse(JSON.stringify(o))

const tokenize = src => {
  const tokens = [{code:'('}]
  let pos = 0
  while (pos < src.length) {
    const matched = src.slice(pos).match(/^[^ \.\n()=+\[\]]+|\n|./)
    if (!matched) { fail('tokenize', {pos, src}) }
    const code = matched[0]
    if (code.match(/\n/)) {
      tokens.push({code:')'})
      tokens.push({code:'('})
    }
    if (code.trim()) {
      tokens.push({code,pos})
    }
    pos += code.length
  }
  tokens.push({code:')'})
  return tokens
}
const parse = src => {
  const tokens = tokenize(src)
  let pos = 0
  const op2s = '+-*/<>.'.split('')
  const op2 = a => {
    if (a.length === 0) { return a }
    const l = a.length
    const r = [a[0]]
    for (let i=1; i<l; i++) {
      const node = a[i]
      const next = a[i+1]
      if (node.code === '=') {
        r.unshift(node)
        node.list = op2(a.slice(i+1))
        r.push(node)
        break
      } else if (op2s.includes(node.code) && next) {
        node.list = [copy(node), r.pop(), next]
        r.push(node)
        i+=1
      } else {
        r.push(node)
      }
    }
    return r
  }
  const pairs = {'(':')', '[':']'}
  const consume = f => f(tokens[pos++] || fail('EOF', {src,pos,tokens}))
  const apply = (p, acc) => (t => t.code === p ? op2(acc) : apply(p, (acc.push(t),acc)))(unit())
  const unit = () => consume(t => {
    const p = pairs[t.code]
    if (p) { t.list = apply(p, []) }
    return t
  })
  const nodes = []
  while (pos < tokens.length) {
    nodes.push(unit())
  }
  return nodes
}
const infer = (nodes,src) => {
  let tvarId = 0
  const trace = x => (dump(x),x)
  const tvar = () => (name => ({name,var:true}))((++tvarId).toString())
  const tlambda = (...types) => ({types:types})
  const ttype = (name) => ({name})
  const tint = ttype('int')
  const tstr = ttype('str')
  const tbool = ttype('bool')
  const tnil = ttype('nil')
  const fresh = (type, nonGeneric) => {
    const d = {}
    const rec = t => {
      const p = prune(t)
      return p.var ?
        (nonGeneric.includes(p.name) ? p : d[p.name]||=tvar()) :
        (p.types ? ({types:p.types.map(rec)}) : ({name:p.name}))
    }
    return rec(type)
  }
  const unify = (a, b) => {
    a = prune(a)
    b = prune(b)
    if (a.var) {
      if (a.name !== b.name) {
        a.instance = b
      }
    } else if (b.var) {
      unify(b, a)
    } else {
      if (a.name !== b.name) { fail(`type miss match`, {a,b}) }
      if (a.types || b.types) {
        if (a.types.length !== b.types.length) { fail('types miss match', {a,b}) }
        a.types.map((t,i) => unify(t, b.types[i]))
      }
    }
  }
  const v1 = tvar()
  let env = {
    'true': tbool,
    'false': tbool,
    '+': tlambda(tint, tint, tint),
    '<': tlambda(tint, tint, tbool),
    'if': tlambda(tbool, v1, v1, v1),
    '__int_string': tlambda(tint, tstr),
  }
  const local = (node, d, nonGeneric) => {
    const keys = Object.keys(d)
    const bk = {}
    keys.map(k => (bk[k]=env[k], env[k]=d[k]))
    const ret = analyse(node, nonGeneric)
    keys.map(k => env[k] = bk[k])
    return ret
  }
  const prune = t => (t.var && t.instance) ? t.instance = prune(t.instance) : t
  const analyse = (node, nonGeneric) => node.type = _analyse(node, nonGeneric)
  const _analyse = (node, nonGeneric) => {
    if (node.list) {
      const list = node.list
      if (list.length === 0) { return tnil }
      let [head,...tail] = list
      if (head.code === '=') {
        if (tail.length < 2) { fail('Not enoug argument', {tail,list}) }
        const name = tail[0].code
        if (tail.length === 2) {
          return tail[0].type = env[name] = analyse(tail[1], nonGeneric)
        } else {
          const args = tail.slice(1,-1)
          const argt = args.map(_ => tvar())
          args.map((a,i) => a.type = argt[i])
          const body = tail.slice(-1)[0]
          const d = dict(args.map(t => t.code), argt)
          const ft = tlambda(...argt, local(body, d, nonGeneric.concat(argt.map(t=>t.name))))
          return tail[0].type  = env[name] = ft
        }
      } else if (head.code === '.') {
        const [dot, lhs, rhs] = head.list
        const rt = tvar()
        const ft = env['__' + analyse(lhs, nonGeneric).name + '_' + rhs.code]
        const args = [lhs].concat(tail)
        unify(tlambda(...args.map(t => analyse(t, nonGeneric)), rt), ft)
        return dot.type = rt
      }
      if (tail.length) {
        const rt = tvar()
        const ft = analyse(head, nonGeneric)
        unify(tlambda(...tail.map(t => analyse(t, nonGeneric)), rt), ft)
        return rt
      } else {
        return head.type = analyse(head, nonGeneric)
      }
    } else {
      const v = node.code
      if (v) {
        if (v.match(/^[0-9]+/)) {
          return tint
        } else if (env[v]) {
          return fresh(env[v], nonGeneric) || fail(`Not found ${v}`, node)
        } else {
          fail("unknown", {v,src,node,env})
        }
      }
    }
  }
  return nodes.map(node => analyse(node, []))
}

const showType = t => {
  const show = t => {
    if (t.instance) {
      return show(t.instance)
    } else if (t.name) {
      return t.name
    } else if (t.types) {
      return '(' + t.types.map(show).join(' ') + ')'
    } else {
      fail("show", t)
    }
  }
  const s = show(t)
  const o = {}
  const r = s.replace(/\d+/g, t => o[t]||=Object.keys(o).length+1)
  return r
}

const showNode = o => {
  const show = o => {
    if (typeof o === 'object' && o.constructor === Array) {
      if (o.length === 0) {
        return '()'
      } else {
        if (o[0].code === '=') {
          return '(= ' + o.slice(1).map(show).join(' ') + ')'
        } else {
          return '(' + o.map(show).join(' ') + ')'
        }
      }
    } else {
      if (!o.type) { fail('untyped', o) }
      return o.code + ':' + showType(o.type)
    }
  }
  return show(o)
}

function testType() {
  const run = src => {
    const nodes = parse(src)
    const types = infer(nodes, src)
    return {nodes, types}
  }
  const reject = (src, expect) => {
    try {
      run(src)
      console.log('Failed')
      console.log('src:', src)
    } catch (e) {
      process.stdout.write('.')
    }
  }
  const inf = (src, expect) => {
    let result
    try {
      result = run(src)
    } catch (e) {
      console.log('Failed')
      console.log('  src:', src)
      console.log('error:', e)
    }
    const actual = showType(result.types.slice(-1)[0])
    if (eq(actual, expect)) {
      process.stdout.write('.')
    } else {
      console.log('Failed')
      console.log('expect:', expect)
      console.log('actual:', actual)
      console.log('   src:', src)
      console.log(' nodes:', result.nodes)
    }
  }
  // lisp style
  inf('+ 1 1', 'int')
  inf('< 1 1', 'bool')
  inf('= f 1\nf', 'int')
  inf('= f a a\nf 1', 'int')
  inf('= f a b a + b\nf 1 2', 'int')
  inf('(+ 1 1)', 'int')
  inf('(< 1 1)', 'bool')
  inf('(false)', 'bool')
  inf('((false))', 'bool')

  // primitives
  inf('1', 'int')
  inf('true', 'bool')
  inf('false', 'bool')

  // embedded
  inf('1 + 1', 'int')
  inf('1 < 1', 'bool')
  inf('if true true true', 'bool')
  inf('if true 1 1', 'int')

  // define
  inf('f = 1\nf', 'int')
  inf('f = 1\nf', 'int')
  inf('f a = a\nf 1', 'int')
  inf('f a b = a + b\nf 1 2', 'int')

  // generics
  inf('f a = a', '(1 1)')
  inf('f a b = a', '(1 2 1)')
  inf('f a b = b', '(1 2 2)')
  inf('f a = a\nf 1\nf true', 'bool')
  inf('f x = x\nif (f true) (f 1) (f 2)', 'int')

  // combinations
  inf('f x = x + 1\n(= g x (+ x 2))\n(+ (f 1) (g 1))', 'int')
  inf('_ f g x = g (f x)', '((1 2) (2 3) 1 3)')
  inf('_ x y z = x z (y z)', '((1 2 3) (1 2) 1 3)')
  inf('_ b x = if (x b) x (= _ x b)', '(1 (1 bool) (1 1))')
  inf('_ x = if true x (if x true false)', '(bool bool)')
  inf('_ x y = if x x y', '(bool bool bool)')
  inf('_ n = (_ x = (x (_ y = y))) (_ f = f n)', '(1 1)')
  inf('_ x y = x y', '((1 2) 1 2)')
  inf('_ x y = x (y x)', '((1 2) ((1 2) 1) 2)')
  inf('_ h t f x = f h (t f x)', '(1 ((1 2 3) 4 2) (1 2 3) 4 3)')
  //inf('(= _ x y (x (y x) (y x)))', '((1 1 2) ((1 1 2) 1) 2)') // TODO: fix

  // class
  inf('1.string', 'str')

  // type errors
  reject('(+ 1 true)')

  print('ok')
}

testType()
