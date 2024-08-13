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
  const allTypes = [tvoid, ...tnum1.interfaces, tbool, tstring, terror, ttuple, tlist, tset, tdict]

  const constructors = {
    list: tfn(v1, {name: 'list', types: [v1]}),         // TODO: variadic arguments
    set: tfn(v1, {name: 'set', types: [v1]}),           // TODO: variadic arguments
    dict: tfn(v1, v2, {name: 'dict', types: [v1, v2]}), // TODO: variadic arguments
    ...Object.fromEntries(tnum1.interfaces.map(t => [t.name, tfn(tnum1, t)]))
  }
  const methods = Object.fromEntries(allTypes.map(t => [t.name, {}]))
  methods.string.size = tint
  methods.string.concat = tfn(tstring, tstring)
  methods.string.reverse = tfn(tstring)
  methods.string.slice = tfn(tint, tint, tstring) // TODO: support variadic arguments
  for (const t of tnum1.interfaces) {
    methods[t.name].abs = tfn(t)
    methods[t.name].neg = tfn(t)
  }
  for (const t of tints1.interfaces) {
    methods[t.name].char = tfn(tstring)
  }
  for (const t of tfloats1.interfaces) {
    methods[t.name].floor = tfn(tint)
    methods[t.name].ceil = tfn(tint)
    methods[t.name].round = tfn(tint)
  }
  const method = (t, name) => (t.__type ? t[name] : methods[t.name]?.[name]) || fail(`${str(t)} has no ${name}`)
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
  const unify = (a, b, node) => {
    a = prune(a)
    b = prune(b)
    if (a.var) {
      if (a.name !== b.name) {
        if (a.interfaces) {
          if (b.interfaces && str(a.interfaces) !== str(b.interfaces)) {
            failUnify(`interfaces miss match`, {a,b,node})
          }
          if (!b.var && !a.interfaces.find(c => c.name === b.name)) {
            failUnify(`interfaces miss match`, {a,b,node})
          }
        }
        if (a.interfaces && b.var) {
          b.instance = a
        } else {
          a.instance = b
        }
      }
    } else if (b.var) {
      unify(b, a, node)
    } else {
      const as = a.types || []
      const bs = b.types || []
      if (a.name !== b.name || (as.length !== bs.length && !as.find(t => t.dot3))) {
        failUnify(`length miss match`, {a,b,node})
      }
      for (let i=0; i<as.length; i++) {
        if (as[i].dot3) {
          const cs = as[i].constrains
          cs && [...Array(bs.length-i-1)].map((_, j) => unify(cs[j%cs.length], bs[i+j], node))
          as[i].ref.instance = {...ttuple, types: bs.slice(i, -1)}
          unify(as.at(-1), bs.at(-1), node)
          break
        } else {
          unify(as[i], bs[i], node)
        }
      }
    }
  }
  const prune = t => (t.var && t.instance) ? t.instance = prune(t.instance) : t
  const lines = x => !Array.isArray(x) ? [[x]] :
    x[0]?.code === '__block' ? x.slice(1).map(x => Array.isArray(x) ? x : [x]) :
    [x]
  const analyse = (node, env, nonGeneric) => node.type = _analyse(node, env, nonGeneric)
  const _analyse = (node, env, nonGeneric) => {
    if (Array.isArray(node)) {
      const [head,...tail] = node
      const has_colon = tail.at(-1)?.[0]?.code === ':'
      const is_typed_name = head.code === 'def' && Array.isArray(tail[0]) // def f[t] ...
      const name = is_typed_name ? tail[0]?.[1]?.code : tail[0]?.code
      const tvars = is_typed_name ? tail[1].concat(tail.slice(1, -1)).map(t => [t.code, tvar()]) :
        tail.slice(0, -1).map(t => [t.code, tvar()])
      const body = has_colon ? tail.at(-1)[1] : tail.at(-1)
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
      } else if (head.code === 'var' || head.code === 'let') {
        const name = tail[0].code
        const rt = tvar()
        env[name] = rt
        unify(rt, analyse(tail[1], env, nonGeneric))
        if (has_colon) {
          analyse(body, env, nonGeneric)
        }
        return rt
      } else if (head.code === 'class') {
        const field = (x, e) => Array.isArray(x) ? tfn(...x.slice(1).flat().map(t => e[t.code])) : e[x.code]
        const props = lines(body).map(a => [
          a[0].code,
          field(a.at(-1), {...Object.fromEntries(a.slice(1, -1).map(t => [t.code, tvar()]).concat(tvars)), ...env})
          ])
        return env[name] = {name, props}
      } else if (head.code === '.') {
        return method(analyse(tail[0], env, nonGeneric), tail[1].code)
      } else if (head.code === '__block') {
        return tail.map(x => analyse(x, env, nonGeneric)).at(-1)
      } else if (head.code === '-' && tail.length === 1) {
        const rt = analyse(tail[0], env, nonGeneric)
        unify(rt, fresh(tnum1, nonGeneric), node)
        return rt
      } else if (tail.length) {
        const ft = analyse(head, env, nonGeneric)
        const argv = tail.map(t => analyse(t, env, nonGeneric))
        if (ft.props) {
          if (ft.props.length !== argv.length) {
            failUnify('length miss match', {ft, argv, node})
          }
          ft.props.map((prop, i) => unify(prop[1], argv[i]), node)
          return {...Object.fromEntries(ft.props), __type: ft}
        } else {
          const rt = argv.find(x => x.var) ? cache[argv.map(x => x.name).join(' ')] ||= tvar() : tvar() // Give signle reference in single context
          unify(constructors[ft.name] || ft, tfn(...argv, rt), node)
          return rt
        }
      } else {
        return analyse(head, env, nonGeneric).types[0]
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
    '~=': tfn(tint, tint, tint),
    '=': tfn(v1, v1),
    'iif': tfn(tbool, v1, v1, v1),
    'assert': tfn(v1, v1, tvoid),
  }
  '|| &&'.split(' ').map(op => top[op] = tfn(tbool, tbool, tbool))
  '+ - * ** / %'.split(' ').map(op => top[op] = tfn(tnum1, tnum1, tnum1))
  '& | ^ << >>'.split(' ').map(op => top[op] = tfn(tints1, tints1, tints1))
  '== != < <= >= >'.split(' ').map(op => top[op] = tfn(v1, v1, tbool))
  '|| && + - * ** / % & | ^ << >>'.split(' ').map(op => top[op+'='] = top[op])
  const primitives = [tint, ti8, ti16, ti32, ti64, tu8, tu16, tu32, tu64, tfloat, tf32, tf64, tbool, ttuple, tlist, tset, tdict]
  primitives.map(t => top[t.name] = t)
  analyse(root, top, [])
  return root
}

module.exports = { infer, TypeError }
