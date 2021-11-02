'use strict'

// Thanks
// https://github.com/reki2000/hyndley-milner-kotlin/blob/master/HindleyMilner.kt
// http://www.fos.kuis.kyoto-u.ac.jp/~igarashi/class/isle4-10w/testcases.html

const print = (...a) => console.log(...a)
const str = o => JSON.stringify(o, null, '  ')
const eq = (x, y) => str(x) === str(y)
const fail = (msg, o) => { throw new Error(msg + ' ' + str(o)) }
const until = (f, g) => {
  const l = []
  while (f()) {
    l.push(g())
  }
  return l
}

const parse = src => {
  const tokens = src.split(/([()]|\s+)/).filter(x => x.trim()).map(code => ({code}))
  let pos = 0
  const list = l => (t => t.code === ')' ? ({list: l}) : list(l.concat([t])))(unit())
  const unit = () => (t => t.code === '(' ? list([]) : t)(tokens[pos++])
  return until(() => pos < tokens.length, unit)
}

const inference = (nodes, src) => {
  let tvarSequence = 0
  const tvar = () => (name => ({name,var:true}))((++tvarSequence).toString())
  const tlambda = (...types) => ({types})
  const ttype = (name) => ({name, types: []})
  const tint = ttype('int')
  const tbool = ttype('bool')
  const fresh = (type, nonGeneric) => {
    const d = {}
    const rec = t => {
      const p = prune(t)
      return p.var ?
        (nonGeneric.includes(p.name) ? p : d[p.name] ||= tvar()) :
        ({name: p.name, types: p.types.map(rec)})
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
      if (a.name !== b.name || a.types.length !== b.types.length) {
        fail(`type miss match`, {a,b})
      }
      a.types.map((t,i) => unify(t, b.types[i]))
    }
  }
  const v1 = tvar()
  const prune = t => (t.var && t.instance) ? t.instance = prune(t.instance) : t
  const analyse = (node, env, nonGeneric) => node.type = _analyse(node, env, nonGeneric)
  const _analyse = (node, env, nonGeneric) => {
    if (node.list) {
      const list = node.list
      let [head,...tail] = list
      if (head.code === 'def') {
        const name = tail[0].code
        const args = tail.slice(1, -1).map(arg => (arg.type = tvar(), arg))
        const body = tail.slice(-1)[0]
        const newEnv = Object.assign({}, env)
        args.map(arg => newEnv[arg.code] = arg.type)
        const ft = tlambda(...args.map(t => t.type), analyse(body, newEnv, nonGeneric.concat(args.map(t => t.type.name))))
        return env[name] = ft
      } else if (tail.length) {
        const argv = tail.map(t => analyse(t, env, nonGeneric))
        const rt = (env.__cache[str(argv)] ||= tvar()) // same signatures should refer a single type
        const ft = analyse(head, env, nonGeneric)
        unify(tlambda(...argv, rt), ft)
        return rt
      } else {
        return analyse(head, env, nonGeneric)
      }
    } else {
      const v = node.code
      return v.match(/^[0-9]/) ? tint :
        env[v] ? fresh(env[v], nonGeneric) :
        fail(`Not found ${v} in env`, {v,src,node,env})
    }
  }
  const topEnv = {
    __cache: {},
    'true': tbool,
    'false': tbool,
    '+': tlambda(tint, tint, tint),
    '<': tlambda(tint, tint, tbool),
    'if': tlambda(tbool, v1, v1, v1),
  }
  return nodes.map(node => analyse(node, topEnv, []))
}

const showType = type => {
  const show = t => t.instance ? show(t.instance) :
    t.name ||
    (t.types.length === 1 ? show(t.types[0]) : '(' + t.types.map(show).join(' ') + ')')
  const s = show(type)
  const o = {}
  const r = s.replace(/\d+/g, t => o[t] ||= Object.keys(o).length + 1)
  return r
}

const testType = () => {
  const reject = src => {
    try {
      inference(parse(src))
    } catch (e) {
      if (e.message.match(/^type miss match/)) {
        process.stdout.write('.')
        return
      }
    }
    print('Failed')
    print('src:', src)
  }
  const inf = (src, expect) => {
    try {
      let types = inference(parse(src))
      const actual = showType(types.slice(-1)[0])
      if (eq(actual, expect)) {
        process.stdout.write('.')
      } else {
        print('Failed')
        print('expect:', expect)
        print('actual:', actual)
        print('   src:', src)
      }
    } catch (e) {
      print('Failed')
      print('  src:', src)
      print('error:', e)
    }
  }
  // primitives
  inf('(1)', 'int')
  inf('(true)', 'bool')
  inf('(false)', 'bool')

  // exp
  inf('(+ 1 1)', 'int')
  inf('(< 1 1)', 'bool')

  // branch
  inf('(if true 1 2)', 'int')
  inf('(if true true true)', 'bool')

  // value
  inf('(def value 1)', 'int')

  // simple function
  inf('(def inc a (+ a 1))', '(int int)')
  inf('(def add a b (+ a b))', '(int int int)')

  // generics
  inf('(def f a a)', '(1 1)')
  inf('(def f a b a)', '(1 2 1)')
  inf('(def f a b b)', '(1 2 2)')
  inf('(def f a a) (f 1)', 'int')
  inf('(def f a a) (f 1) (f true)', 'bool')

  // combinations
  inf('(def f x (+ x 1)) (def g x (+ x 2)) (+ (f 1) (g 1))', 'int')
  inf('(def _ f g x (g (f x)))', '((1 2) (2 3) 1 3)')
  inf('(def _ x y z (x z (y z)))', '((1 2 3) (1 2) 1 3)')
  inf('(def _ b x (if (x b) x (def _ x b)))', '(1 (1 bool) (1 1))')
  inf('(def _ x (if true x (if x true false)))', '(bool bool)')
  inf('(def _ x y (if x x y))', '(bool bool bool)')
  inf('(def _ n ((def _ x (x (def _ y y))) (def _ f (f n))))', '(1 1)')
  inf('(def _ x y (x y))', '((1 2) 1 2)')
  inf('(def _ x y (x (y x)))', '((1 2) ((1 2) 1) 2)')
  inf('(def _ h t f x (f h (t f x)))', '(1 ((1 2 3) 4 2) (1 2 3) 4 3)')
  inf('(def _ x y (x (y x) (y x)))', '((1 1 2) ((1 1 2) 1) 2)')
  inf('(def id x x) (def f y (id (y id)))', '(((1 1) 2) 2)')
  inf('(def id x x) (def f (if (id true) (id 1) (id 2)))', 'int')
  inf('(def f x (3)) (def g (+ (f true) (f 4)))', 'int')
  inf('(def f x x) (def g y y) (def h b (if b (f g) (g f)))', '(bool (1 1))')

  // type errors
  reject('(+ 1 true)')

  print('ok')
}

testType()
