'use strict'
// Infer type for AST
// Thanks
// https://github.com/reki2000/hyndley-milner-kotlin/blob/master/HindleyMilner.kt
// http://www.fos.kuis.kyoto-u.ac.jp/~igarashi/class/isle4-10w/testcases.html
class TypeError extends Error {}
const log = o => { console.dir(o, {depth: null}); return o }
const str = o => JSON.stringify(o, null, '  ')
const fail = (m, ...a) => { const e = new Error(m); a && (e.detail = a); throw e }
const failUnify = (m, ...a) => { const e = new TypeError(m); a && (e.detail = a); throw e }
const until = (f, g) => {
  const l = []
  while (f()) {
    l.push(g())
  }
  return l
}

const infer = root => {
  let tvarSequence = 0
  const cache = {}
  const tvar = (label, interfaces) => (name => ({name, label, interfaces, var: true}))((++tvarSequence).toString())
  const v1 = tvar()
  const v2 = tvar()
  const tdot3 = () => ({dot3: true, ref: tvar()})
  const tdot3_1 = tdot3()
  const tfn = (...types) => ({types})
  const ttype = (name, ...types) => types.length ? ({name, types}) : ({name})

  const tvoid = ttype('_')
  const tint = ttype('int')
  const ti8 = ttype('i8')
  const ti16 = ttype('i16')
  const ti32 = ttype('i32')
  const ti64 = ttype('i64')
  const tu8 = ttype('u8')
  const tu16 = ttype('u16')
  const tu32 = ttype('u32')
  const tu64 = ttype('u64')
  const tfloat = ttype('float')
  const tf32 = ttype('f32')
  const tf64 = ttype('f64')
  const tbool = ttype('bool')
  const tstring = ttype('string')
  const terror = ttype('error', v1)
  const ttuple = ttype('tuple')
  const tlist = ttype('list', v1)
  const tset = ttype('set', v1)
  const tdict = ttype('dict', v1, v2)

  const tints1 = tvar('ints', [tint, ti8, ti16, ti32, ti64, tu8, tu16, tu32, tu64])
  const tfloats1 = tvar('floats', [tfloat, tf32, tf64])
  const tnum1 = tvar('num', [...tints1.interfaces, ...tfloats1.interfaces])

  const constructors = {
    list: tfn(v1, {name: 'list', types: [v1]}),         // TODO: variadic arguments
    set: tfn(v1, {name: 'set', types: [v1]}),           // TODO: variadic arguments
    dict: tfn(v1, v2, {name: 'dict', types: [v1, v2]}), // TODO: variadic arguments
    ...Object.fromEntries(tnum1.interfaces.map(t => [t.name, tfn(tnum1, t)]))
  }
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
          as[i].ref.instance = {...ttuple, types: bs.slice(i, -1)}
          unify(as.at(-1), bs.at(-1))
          break
        } else {
          unify(as[i], bs[i])
        }
      }
    }
  }
  const prune = t => (t.var && t.instance) ? t.instance = prune(t.instance) : t
  const nest = x => !Array.isArray(x) ? [[x]] :
    x[0]?.code === '__block' ? x.slice(1).map(x => Array.isArray(x) ? x : [x]) :
    [x]
  const analyse = (node, env, nonGeneric) => node.type = _analyse(node, env, nonGeneric)
  const _analyse = (node, env, nonGeneric) => {
    if (Array.isArray(node)) {
      const [head,...tail] = node
      const is_block = tail.at(-1)?.[0]?.code === ':'
      const is_typed_name = head.code === 'def' && Array.isArray(tail[0]) // def f[t] ...
      const name = is_typed_name ? tail[0]?.[1]?.code : tail[0]?.code
      const tvars = is_typed_name ? tail[1].concat(tail.slice(1, -1)).map(t => [t.code, tvar()]) :
        tail.slice(0, -1).map(t => [t.code, tvar()])
      const body = is_block ? tail.at(-1)[1] : tail.at(-1)
      if (head.code === 'def') {
        const tenv = {...env, ...Object.fromEntries(tvars)}
        const argument = arg => Array.isArray(arg) ?
          [arg[1].code.replace('...', ''), arg[0].type = tcon(arg[1].code.startsWith('...') ? tdot3() : tvar(), arg.slice(2).map(t => tenv[t.code]))] :
          [arg.code.replace('...', ''), arg.type = arg.code.startsWith('...') ? tdot3() : tvar()]
        const args = tail.slice(1, -1).map(argument)
        const local = {...tenv, ...Object.fromEntries(args.map(([name, t]) => [name, t.dot3 ? t.ref : t]))}
        const ng = [...nonGeneric, ...[...tvars, ...args].map(([_, t]) => t.dot3 ? t.ref.name : t.name)]
        const rt = analyse(body, local, ng)
        const ft = tfn(...args.map(([_, t]) => t), rt)
        return env[name] = ft
      } else if (head.code === 'dec') {
        const local = {...env, ...Object.fromEntries(tvars)}
        return env[name] = tfn(...([].concat(body).map(arg => local[arg.code])))
      } else if (head.code === 'class') {
        const field = (x, e) => Array.isArray(x) ? tfn(...x.slice(1).flat().map(t => e[t.code])) : e[x.code]
        const props = nest(body).map(a => [
          a[0].code,
          field(a.at(-1), {...Object.fromEntries(a.slice(1, -1).map(t => [t.code, tvar()]).concat(tvars)), ...env})
          ])
        return env[name] = {name, props}
      } else if (head.code === '.') {
        return analyse(tail.slice(0, -1), env, nonGeneric)[tail.at(-1).code]
      } else if (head.code === '__block') {
        return tail.map(x => analyse(x, env, nonGeneric)).at(-1)
      } else if (tail.length) {
        const ft = analyse(head, env, nonGeneric)
        const argv = tail.map(t => analyse(t, env, nonGeneric))
        if (ft.props) {
          if (ft.props.length !== argv.length) {
            failUnify('length miss match', {ft, argv, node})
          }
          ft.props.map((prop, i) => unify(prop[1], argv[i]))
          return Object.fromEntries(ft.props)
        } else if (ft.name in constructors) {
          const rt = cache[str(argv)] ||= tvar() // TODO: fix tvar
          unify(constructors[ft.name], tfn(...argv, rt))
          return rt
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
        v.startsWith('"') ? tstring :
        v in env ? fresh(env[v], nonGeneric) :
        fail(`Not found '${v}' in env`, {v,node,env: Object.keys(env)})
    }
  }
  const top = {
    '...': tdot3(),
    'true': tbool,
    'false': tbool,
    '!': tfn(tbool, tbool),
    '~': tfn(tint, tint),
    'iif': tfn(tbool, v1, v1, v1),
    'assert': tfn(v1, v1, tvoid),
  }
  '|| &&'.split(' ').map(op => top[op] = tfn(tbool, tbool, tbool))
  '+ - * ** / %'.split(' ').map(op => top[op] = tfn(tnum1, tnum1, tnum1))
  '& | ^ ~ << >>'.split(' ').map(op => top[op] = tfn(tints1, tints1, tints1))
  '== != < <= >= >'.split(' ').map(op => top[op] = tfn(v1, v1, tbool))
  const primitives = [tint, ti8, ti16, ti32, ti64, tu8, tu16, tu32, tu64, tfloat, tf32, tf64, tbool, ttuple, tlist, tset, tdict]
  primitives.map(t => top[t.name] = t)
  analyse(root, top, [])
  return root
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
  const r = s.replaceAll(/\b\d+/g, t => o[t] ||= Object.keys(o).length + 1)
  return r
}

module.exports = { infer }

if (require.main === module) {
  const { parse } = require('./parse.js')
  const reject = src => {
    try {
      infer(parse(src))
    } catch (e) {
      if (e instanceof TypeError) {
        process.stdout.write('.')
        return
      }
    }
    console.log('Invalid')
    console.log('src:', src)
    process.exit(1)
  }
  const inf = (expect, src) => {
    try {
      const node = infer(parse(src))
      const actual = showType(node.type)
      if (str(actual) === str(expect)) {
        process.stdout.write('.')
      } else {
        console.log('Failed')
        console.log('expect:', expect)
        console.log('actual:', actual)
        console.log('   src:', src)
        process.exit(1)
      }
    } catch (e) {
      console.log('Failed')
      console.log('   src:', src)
      console.dir(e, {depth: null})
      process.exit(1)
    }
  }
  inf('tuple[int bool int bool]', 'def f[t u] ...a[t u]: a\nf 1 true 2 false')

  // literal
  inf('bool', 'true')
  inf('bool', 'false')
  inf('int', '1')
  inf('float', '1.0')

  // constructor of numbers
  'int i8 i16 i32 i64 u8 u16 u32 u64 float f32 f64'.split(' ').map(t => inf(t, `${t} 0`))

  // constructor of collections
  inf('list[int]', 'list 1')
  inf('set[int]', 'set 1')
  inf('dict[int bool]', 'dict 1 true')

  // exp
  inf('int', '+ 1 1')
  inf('float', '+ 1.0 1.0')
  inf('bool', '< 1 1')

  // branch
  inf('int', 'iif true 1 2')
  inf('bool', 'iif true true true')

  // value
  inf('(int)', 'def _: 1')

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
  inf('(int)',                         'def id x x\ndef f: iif (id true) (id 1) (id 2)')
  inf('(int)',                         'def f x (3)\ndef g: + (f true) (f 4)')
  inf('(bool (1 1))',                  'def f x x\ndef g y y\ndef h b (iif b (f g) (g f))')

  // declare function
  inf('(bool)', 'dec _: bool')
  inf('(int)', 'dec _: int')
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
  inf('(x.1)', 'class _ a: x a')
  inf('(f.(int))', 'class _: f : int')
  inf('(f.(int int))', 'class _: f : int int')
  inf('(f.(1 2 int))', 'class _ a: f b: a b int')
  inf('(f.(1 2 3 4))', 'class _ a b: f c d: a b c d')

  // type errors
  reject('(+ true true)')
  reject('(+ 1 true)')
  reject('(+ 1 1.0)')
  reject('def f[t] ...[t]: true\nf 1 true')
  reject('def f[t u] ...[t u]: true\nf 1 true true')

  console.log('ok')
}
