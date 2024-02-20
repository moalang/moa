'use strict'

// Thanks
// https://github.com/reki2000/hyndley-milner-kotlin/blob/master/HindleyMilner.kt
// http://www.fos.kuis.kyoto-u.ac.jp/~igarashi/class/isle4-10w/testcases.html

const log = o => { console.dir(o, {depth: null}); return o }
const print = (...a) => console.log(...a)
const str = o => JSON.stringify(o, null, '  ')
const eq = (x, y) => str(x) === str(y)
const fail = (m, ...a) => { const e = new Error(m); a && (e.detail = a); throw e }
const until = (f, g) => {
  const l = []
  while (f()) {
    l.push(g())
  }
  return l
}

const infer = nodes => {
  let tvarSequence = 0
  const cache = {}
  const tvar = () => (name => ({name, var: true}))((++tvarSequence).toString())
  const tfn = (...types) => types.length === 1 ? types[0] : ({types})
  const tinf = props => ({props})
  const ttype = name => ({name})
  const ttuple = types => ({name: 'tuple', types})
  const tdot3 = {name: '...', var: true}
  const tint = ttype('int')
  const tbool = ttype('bool')
  const fresh = (type, nonGeneric) => {
    const d = {}
    const rec = t => {
      const p = prune(t)
      return p.var ?
        (nonGeneric.includes(p.name) ? p : d[p.name] ||= tvar()) :
        p.name && p.types ? ({name: p.name, types: p.types.map(rec)}) :
        p.types ? ({types: p.types.map(rec)}) :
        p
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
      const as = a.types || []
      const bs = b.types || []
      if (a.name !== b.name || (as.length !== bs.length && !as.find(t => t.name === '...'))) {
        fail(`type miss match`, {a,b})
      }
      as.some((t,i) =>
        t.name === '...' ? (t.instance = ttuple(bs.slice(i, -1)), unify(as.at(-1), bs.at(-1)), true) :
        unify(t, bs[i]))
    }
  }
  const v1 = tvar()
  const prune = t => (t.var && t.instance) ? t.instance = prune(t.instance) : t
  const analyse = (node, env, nonGeneric) => node.type = _analyse(node, env, nonGeneric)
  const _analyse = (node, env, nonGeneric) => {
    if (Array.isArray(node)) {
      let [head,...tail] = node
      if (head.code === 'def') {
        const tvars = (Array.isArray(tail[0]) ? tail[0] : [tail[0]]).map(t => [t.code, tvar()])
        const name = tvars[0][0]
        const tenv = {...env, ...Object.fromEntries(tvars)}
        const args = tail.slice(1, -1).map(arg => [arg.code.replace('...', ''), arg.type = arg.code.startsWith('...') ? tdot3 : tvar()])
        const body = tail.slice(-1)[0]
        const local = {...tenv, ...Object.fromEntries(args)}
        const ret = analyse(body, local, [...nonGeneric, ...[...tvars, ...args].map(([_, t]) => t.name)])
        const ft = tfn(...args.map(([_, t]) => t), ret)
        return env[name] = ft
      } else if (head.code === 'dec') {
        const tvars = (Array.isArray(tail[0]) ? tail[0] : [tail[0]]).map(t => [t.code, tvar()])
        const name = tvars[0][0]
        const local = {...env, ...Object.fromEntries(tvars)}
        if (Array.isArray(tail.at(-1))) {
          const props = tail.at(-1).map(a => [a[0].code, tfn(...a.slice(1).map(t => local[t.code]))])
          return env[name] = tinf(props)
        } else {
          const args = tail.slice(1).map(arg => local[arg.code])
          return env[name] = args.length === 1 ? args[0] : tfn(...args)
        }
      } else if (tail.length) {
        const argv = tail.map(t => analyse(t, env, nonGeneric))
        const rt = cache[str(argv)] ||= tvar() // fix tvar
        const ft = analyse(head, env, nonGeneric)
        unify(ft, tfn(...argv, rt))
        return rt
      } else {
        return analyse(head, env, nonGeneric)
      }
    } else {
      const v = node.code
      return v.match(/^[0-9]/) ? tint :
        v in env ? fresh(env[v], nonGeneric) :
        fail(`Not found ${v} in env`, {v,node,env})
    }
  }
  const top = {
    '...': tdot3,
    'int': tint,
    'bool': tbool,
    'true': tbool,
    'false': tbool,
    '+': tfn(tint, tint, tint),
    '<': tfn(tint, tint, tbool),
    'if': tfn(tbool, v1, v1, v1),
  }
  return nodes.map(node => analyse(node, top, ['...']))
}

const showType = type => {
  const show = t => t.instance ? show(t.instance) :
    t.name && t.types ? t.name + '[' + t.types.map(show).join(' ') + ']' :
    t.types ? '(' + t.types.map(show).join(' ') + ')' :
    t.props ? '(' + t.props.map(([f,t]) => `${f}.${show(t)}`).join(' ') + ')' :
    t.name
  const s = show(type)
  const o = {}
  const r = s.replace(/\d+/g, t => o[t] ||= Object.keys(o).length + 1)
  return r
}

const testType = () => {
  const parse = src => {
    const tokens = src.split(/([()\[\]:;]|\s+)/).filter(x => x.trim()).map(code => ({code}))
    let pos = 0
    const check = f => pos < tokens.length && f(tokens[pos].code)
    const list = a => (t => t.code === ')' ? a : list(a.concat([t])))(unit())
    const bracket = a => (t => t.code === ']' ? a : bracket(a.concat([t])))(unit())
    const suffix = t =>
      check(s => s === '[') && ++pos ? bracket([t]) :
      t
    const unit = () => (t =>
      t.code === '(' ? list([]) :
      t.code === ':' ? block([top()]) :
      suffix(t))(tokens[pos++])
    const block = a => check(s => s === ';') ? (++pos, block([...a, top()])) : a
    const top = () => until(() => check(s => s !== ')' && s !== ';'), unit)
    return block([top()])
  }

  const reject = src => {
    try {
      infer(parse(src))
    } catch (e) {
      if (e.message.match(/^type miss match/)) {
        process.stdout.write('.')
        return
      }
    }
    print('Failed')
    print('src:', src)
  }
  const inf = (expect, src) => {
    try {
      let types = infer(parse(src))
      const actual = showType(types.slice(-1)[0])
      if (eq(actual, expect)) {
        process.stdout.write('.')
      } else {
        print('Failed')
        print('expect:', expect)
        print('actual:', actual)
        print('   src:', src)
        process.exit(1)
      }
    } catch (e) {
      print('Failed')
      print('  src:', src)
      console.dir(e, {depth: null})
      process.exit(1)
    }
  }

  // primitives
  inf('int', '1')
  inf('bool', 'true')
  inf('bool', 'false')

  // exp
  inf('int', '+ 1 1')
  inf('bool', '< 1 1')

  // branch
  inf('int', 'if true 1 2')
  inf('bool', 'if true true true')

  // value
  inf('int', 'def _ 1')

  // simple function
  inf('(int int)', 'def _ a (+ a 1)')
  inf('(int int int)', 'def _ a b (+ a b)')

  // generics
  inf('(1 1)', 'def _ a a')
  inf('(1 2 1)', 'def _ a b a')
  inf('(1 2 2)', 'def _ a b b')
  inf('int', 'def f a a; f 1')
  inf('bool', 'def f a a; f 1; f true')

  // combinations
  inf('int',                           'def f x (+ x 1); def g x (+ x 2); + (f 1) (g 1)')
  inf('((1 2) (2 3) 1 3)',             'def _ f g x (g (f x))')
  inf('((1 2 3) (1 2) 1 3)',           'def _ x y z (x z (y z))')
  inf('(1 (1 bool) (1 1))',            'def _ b x (if (x b) x (def _ x b))')
  inf('(bool bool)',                   'def _ x (if true x (if x true false))')
  inf('(bool bool bool)',              'def _ x y (if x x y)')
  inf('(1 1)',                         'def _ n ((def _ x (x (def _ y y))) (def _ f (f n)))')
  inf('((1 2) 1 2)',                   'def _ x y (x y)')
  inf('((1 2) ((1 2) 1) 2)',           'def _ x y (x (y x))')
  inf('(1 ((1 2 3) 4 2) (1 2 3) 4 3)', 'def _ h t f x (f h (t f x))')
  inf('((1 1 2) ((1 1 2) 1) 2)',       'def _ x y (x (y x) (y x))')
  inf('(((1 1) 2) 2)',                 'def id x x; def f y (id (y id))')
  inf('int',                           'def id x x; def f (if (id true) (id 1) (id 2))')
  inf('int',                           'def f x (3); def g (+ (f true) (f 4))')
  inf('(bool (1 1))',                  'def f x x; def g y y; def h b (if b (f g) (g f))')

  // type errors
  reject('(+ 1 true)')

  // declare function
  inf('bool', 'dec f bool; f')
  inf('int', 'dec _ int')
  inf('(int bool)', 'dec _ int bool')
  inf('(... int)', 'dec _ ... int')
  inf('(1 1)', 'dec _[a] a a')
  inf('(1 1 2)', 'dec _[a b] a a b')

  // declare object
  inf('(x.int)', 'dec _: x int')
  inf('(x.1)', 'dec _[a]: x a')
  inf('(f.(int int))', 'dec _: f int int')
  inf('(x.1 f.(1 2 int))', 'dec _[a b]: x a; f a b int')

  // variadic arguments
  inf('(... int)', 'def f ... 1')
  inf('int', 'def f ... 1; f 1')
  inf('int', 'def f ... 1; f 1 true')
  inf('(... ...)', 'def f ...a a')
  inf('tuple[int]', 'def f ...a a; f 1')
  inf('tuple[int bool]', 'def f ...a a; f 1 bool')
  //inf('bool', 'def f[t] ...[t] true; f 1 2')

  print('ok')
}

testType()
