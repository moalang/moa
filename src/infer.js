'use strict'

// Thanks
// https://github.com/reki2000/hyndley-milner-kotlin/blob/master/HindleyMilner.kt
// http://www.fos.kuis.kyoto-u.ac.jp/~igarashi/class/isle4-10w/testcases.html

class TypeError extends Error {}
const log = o => { console.dir(o, {depth: null}); return o }
const print = (...a) => console.log(...a)
const str = o => JSON.stringify(o, null, '  ')
const eq = (x, y) => str(x) === str(y)
const fail = (m, ...a) => { const e = new Error(m); a && (e.detail = a); throw e }
const failUnify = (m, ...a) => { const e = new TypeError(m); a && (e.detail = a); throw e }
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
  const tvar = (label, interfaces) => (name => ({name, label, interfaces, var: true}))((++tvarSequence).toString())
  const tdot3 = () => ({dot3: true, ref: tvar()})
  const tfn = (...types) => types.length === 1 ? types[0] : ({types})
  const tclass = (name, props) => ({name, props})
  const ttype = name => ({name})
  const ttuple = types => ({name: 'tuple', types})
  const tint = ttype('int')
  const tfloat = ttype('float')
  const tbool = ttype('bool')
  const tnum1 = tvar('num', [tint, tfloat])
  const tcon = (t, constrains) => ({...t, constrains})
  const fresh = (type, nonGeneric) => {
    const d = {}
    const rec = t => {
      const copy = t => {
        t = {...t}
        t.constrains &&= t.constrains.map(rec)
        t.types &&= t.types.map(rec)
        t.ref &&= rec(t.ref)
        return t
      }
      const p = prune(t)
      return p.var ? (nonGeneric.includes(p.name) ? p : d[p.name] ||= tvar(p.label, p.interfaces)) : copy(p)
    }
    return rec(type)
  }
  const unify = (a, b) => {
    a = prune(a)
    b = prune(b)
    if (a.var) {
      if (a.name !== b.name) {
        if (a.interfaces) {
          if (b.interfaces && str(a.interfaces) !== str(b.interfaces)) {
            failUnify(`interfaces miss match`, {a,b})
          }
          if (!b.var && !a.interfaces.find(c => c.name === b.name)) {
            failUnify(`interfaces miss match`, {a,b})
          }
        }
        if (a.interfaces && b.var) {
          b.instance = a
        } else {
          a.instance = b
        }
      }
    } else if (b.var) {
      unify(b, a)
    } else {
      const as = a.types || []
      const bs = b.types || []
      if (a.name !== b.name || (as.length !== bs.length && !as.find(t => t.dot3))) {
        failUnify(`length miss match`, {a,b})
      }
      for (let i=0; i<as.length; i++) {
        if (as[i].dot3) {
          const cs = as[i].constrains
          cs && [...Array(bs.length-i-1)].map((_, j) => unify(cs[j%cs.length], bs[i+j]))
          as[i].ref.instance = ttuple(bs.slice(i, -1))
          unify(as.at(-1), bs.at(-1))
          break
        } else {
          unify(as[i], bs[i])
        }
      }
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
        const argument = arg => Array.isArray(arg) ?
          [arg[0].code.replace('...', ''), arg[0].type = tcon(arg[0].code.startsWith('...') ? tdot3() : tvar(), arg.slice(1).map(t => tenv[t.code]))] :
          [arg.code.replace('...', ''), arg.type = arg.code.startsWith('...') ? tdot3() : tvar()]
        const args = tail.slice(1, -1).map(argument)
        const body = tail.slice(-1)[0]
        const local = {...tenv, ...Object.fromEntries(args.map(([name, t]) => [name, t.dot3 ? t.ref : t]))}
        const ng = [...nonGeneric, ...[...tvars, ...args].map(([_, t]) => t.dot3 ? t.ref.name : t.name)]
        const rt = analyse(body, local, ng)
        const ft = tfn(...args.map(([_, t]) => t), rt)
        return env[name] = ft
      } else if (head.code === 'dec') {
        const tvars = tail.slice(0, -1).map(t => [t.code, tvar()])
        const name = tvars[0][0]
        const local = {...env, ...Object.fromEntries(tvars)}
        const args = tail.at(-1)[0].map(arg => local[arg.code])
        return env[name] = args.length === 1 ? args[0] : tfn(...args)
      } else if (head.code === 'class') {
        const tvars = tail.slice(0, -1).map(t => [t.code, tvar()])
        const name = tvars[0][0]
        const local = {...env, ...Object.fromEntries(tvars)}
        const props = tail.at(-1).map(a => [a[0].code, tfn(...a.slice(1).map(t => local[t.code]))])
        return env[name] = tclass(name, props)
      } else if (head.code === '.') {
        return analyse(tail.slice(0, -1), env, nonGeneric)[tail.at(-1).code]
      } else if (tail.length) {
        const ft = analyse(head, env, nonGeneric)
        const argv = tail.map(t => analyse(t, env, nonGeneric))
        if (ft.props) {
          if (ft.props.length !== argv.length) {
            failUnify('length miss match', {ft, argv, node})
          }
          ft.props.map((prop, i) => unify(prop[1], argv[i]))
          return Object.fromEntries(ft.props)
        } else {
          const rt = cache[str(argv)] ||= tvar() // TODO: fix tvar
          unify(ft, tfn(...argv, rt))
          return rt
        }
      } else {
        return analyse(head, env, nonGeneric)
      }
    } else {
      const v = node.code
      return v.match(/^[0-9]+\.[0-9]+/) ? tfloat :
        v.match(/^[0-9]/) ? tint :
        v in env ? fresh(env[v], nonGeneric) :
        fail(`Not found ${v} in env`, {v,node,env})
    }
  }
  const top = {
    '...': tdot3(),
    'int': tint,
    'float': tfloat,
    'bool': tbool,
    'true': tbool,
    'false': tbool,
    '!': tfn(tbool, tbool),
    'iif': tfn(tbool, v1, v1, v1),
  }
  '|| &&'.split(' ').map(op => top[op] = tfn(tbool, tbool, tbool))
  '+ - * ** / %'.split(' ').map(op => top[op] = tfn(tnum1, tnum1, tnum1))
  '& | ^ ~ << >>'.split(' ').map(op => top[op] = tfn(tint, tint, tint))
  '!= == < <= >= >'.split(' ').map(op => top[op] = tfn(v1, v1, tbool))
  return nodes.map(node => analyse(node, top, []))
}

const showType = type => {
  const show = t => t.instance ? show(t.instance) :
    t.dot3 ? '...' :
    t.name && t.types ? t.name + '[' + t.types.map(show).join(' ') + ']' :
    t.types ? '(' + t.types.map(show).join(' ') + ')' :
    t.props ? '(' + t.props.map(([f,t]) => `${f}.${show(t)}`).join(' ') + ')' :
    t.name + (t.label ? '.' + t.label : '')
  const s = show(type)
  const o = {}
  const r = s.replace(/\d+/g, t => o[t] ||= Object.keys(o).length + 1)
  return r
}

const testType = () => {
  const parse = src => {
    const line = src => {
      const tokens = src.split(/([()\[\]:;]|(?=[ \n])\.(?= )|\s+)/).filter(x => x.trim()).map(code => ({code}))
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
    return src.split('\n').map(line).flat(1)
  }

  const reject = src => {
    try {
      infer(parse(src))
    } catch (e) {
      if (e instanceof TypeError) {
        process.stdout.write('.')
        return
      }
    }
    print('Invalid')
    print('src:', src)
    process.exit(1)
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
      print('   src:', src)
      console.dir(e, {depth: null})
      process.exit(1)
    }
  }

  // primitives
  inf('bool', 'true')
  inf('bool', 'false')
  inf('int', '1')
  inf('float', '1.0')

  // exp
  inf('int', '+ 1 1')
  inf('float', '+ 1.0 1.0')
  inf('bool', '< 1 1')

  // branch
  inf('int', 'iif true 1 2')
  inf('bool', 'iif true true true')

  // value
  inf('int', 'def _: 1')

  // simple function
  inf('(int int)', 'def _ a: + a 1')
  inf('(1.num 1.num 1.num)', 'def _ a b: + a b')

  // generics
  inf('(1 1)', 'def _ a: a')
  inf('(1 2 1)', 'def _ a b: a')
  inf('(1 2 2)', 'def _ a b: b')
  inf('int', 'def f a: a\nf 1')
  inf('bool', 'def f a: a\nf 1\nf true')

  // combinations
  inf('int',                           'def f x: + x 1\ndef g x: + x 2\n+ (f 1) (g 1)')
  inf('((1 2) (2 3) 1 3)',             'def _ f g x: g (f x)')
  inf('((1 2 3) (1 2) 1 3)',           'def _ x y z: x z (y z)')
  inf('(1 (1 bool) (1 1))',            'def _ b x: iif (x b) x (def _ x: b)')
  inf('(bool bool)',                   'def _ x: iif true x (iif x true false)')
  inf('(bool bool bool)',              'def _ x y: iif x x y')
  inf('(1 1)',                         'def _ n: (def _ x: x (def _ y y)) (def _ f: f n)')
  inf('((1 2) 1 2)',                   'def _ x y: x y')
  inf('((1 2) ((1 2) 1) 2)',           'def _ x y: x (y x)')
  inf('(1 ((1 2 3) 4 2) (1 2 3) 4 3)', 'def _ h t f x: f h (t f x)')
  inf('((1 1 2) ((1 1 2) 1) 2)',       'def _ x y: x (y x) (y x)')
  inf('(((1 1) 2) 2)',                 'def id x: x\ndef f y: id (y id)')
  inf('int',                           'def id x x\ndef f (iif (id true) (id 1) (id 2))')
  inf('int',                           'def f x (3)\ndef g (+ (f true) (f 4))')
  inf('(bool (1 1))',                  'def f x x\ndef g y y\ndef h b (iif b (f g) (g f))')

  // declare function
  inf('bool', 'dec _: bool')
  inf('int', 'dec _: int')
  inf('(int bool)', 'dec _: int bool')
  inf('(... int)', 'dec _: ... int')
  inf('(1 1)', 'dec _ a: a a')
  inf('(1 1 2)', 'dec _ a b: a a b')

  // variadic arguments
  inf('(... int)', 'def f ...: 1')
  inf('int', 'def f ...: 1\nf 1')
  inf('int', 'def f ...: 1\nf 1 true')
  inf('(... 1)', 'def f ...a: a')
  inf('tuple[int]', 'def f ...a: a\nf 1')
  inf('tuple[int bool]', 'def f ...a: a\nf 1 bool')
  inf('bool', 'def f[t] ...[t]: true\nf 1 2')
  inf('int', 'def f[t] ...[t]: t\nf 1 2')
  inf('tuple[int bool int bool]', 'def f[t u] ...a[t u]: a\nf 1 true 2 false')

  // class
  inf('(x.int)', 'class _: x int')
  inf('int', 'class c: x int\n. c(1) x')
  inf('(x.1)', 'class _ a: x a')
  inf('int', 'class c a: x a\n. c(1) x')
  inf('bool', 'class c a: x a\n. c(true) x')
  inf('(f.(int int))', 'class _: f int int')
  inf('(x.1 f.(1 2 int))', 'class _ a b: x a; f a b int')

  // type errors
  reject('(+ true true)')
  reject('(+ 1 true)')
  reject('(+ 1 1.0)')
  reject('def f[t] ...[t]: true\nf 1 true')
  reject('def f[t u] ...[t u]: true\nf 1 true true')

  print('ok')
}

testType()
