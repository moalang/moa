"use strict"
import {newtoken} from "./parse.js"

const range = n => [...new Array(n)].map((_, i) => i)

class TypeError extends Error { }

const infer = root => {
  let varId = 0
  const newtype = (name, types=[]) => ({name, types})
  const newvar = () => ({...newtype(varId++), var: true})
  const newfn = (types) => newtype("fn", types)
  const newstruct = (name, types) => ({...newfn(types.slice(0, -1).concat([{...types.at(-1), struct: name}])), struct: name})
  const newtuple = types => newtype("tuple", types)
  const newnew = fields => ({...newtype("new" + fields.map(a => "__" + a.code).join(""), fields.map(a => a.type)), fields})
  const newvec = t => newtype("vec", [t])
  const newset = t => newtype("set", [t])
  const newmap = (k, v) => newtype("map", [k, v])

  const marks = (t, mark) => !mark ? t : mark.includes("*") ? ({...newvec(t), mark}) : ({...t, mark})
  const property = (t, name) => (t.props ||= {}, t.props[name] = newvar())

  const faketoken = (s, type) => ({...newtoken(s, "buildin"), type})
  const prune = x => x.instance ? x.instance = prune(x.instance) : x
  const tvoid = newtype("struct{}")
  const tbool = newtype("bool")
  const tint = newtype("int")
  const tfloat = newtype("float64")
  const tstring = newtype("string")
  const terror = newtype("error")
  const tbytes = newtype("bytes")
  const trequest = {...newnew([
    faketoken("scheme", tstring),
    faketoken("method", tstring),
    faketoken("path", tstring),
    faketoken("version", tstring),
    faketoken("host", tstring),
    faketoken("ip", tstring),
  ]), struct: "__io_request"}
  const tresponse = {...newnew([
    faketoken("text", newfn([tstring])),
    faketoken("header", newfn([tstring, tstring])),
    faketoken("cookie", newfn([tstring, tstring])),
  ]), struct: "__io_response"}
  const trequestinit = {...newnew([
    faketoken("method", tstring),
    faketoken("headers", newmap(tstring, newvec(tstring))),
    faketoken("body", tbytes),
  ]), struct: "__io_request_init"}
  const tresponseinit = {...newnew([
    faketoken("status", tint),
    faketoken("headers", newmap(tstring, newvec(tstring))),
    faketoken("blob", tbytes),
    faketoken("json", tbytes),
    faketoken("html", tstring),
    faketoken("text", tstring),
  ]), struct: "__io_response_init"}
  const tio = newtype("io")
  const tv1 = newvar()
  const tv2 = newvar()
  const env = {
    "!": newfn([tbool, tbool]),
    "return": newfn([tv1, tv1]),
    "if": newfn([tbool, tv1, marks(tv2, "?"), tvoid]),
    "iif": newfn([tbool, tv1, tv1, tv1]),
    "throw": newfn([tv1, tv1]),
    "catch": newfn([tv1, newfn([terror, tv1]), tv1]),
    "while": newfn([tbool, tv1, tvoid]),
    "bool": tbool,
    "int": tint,
    "float": tfloat,
    "string": tstring,
    "io": tio,
    "bytes": tbytes,
    "async": newfn([tv1, tvoid]),
    "continue": tvoid,
    "break": tvoid,
  }

  const method = (t, field) =>
    t.var ? property(t, field) :
    t.name === "tuple" && field < t.types.length ? t.types[field] :
    t.name === "error" && field === "message" ? tstring :
    t.name === "io" && field === "sleep" ? newfn([tfloat, tvoid]) :
    t.name === "io" && field === "put" ? newfn([newvar(), tint]) :
    t.name === "io" && field === "puts" ? newfn([newvar(), tint]) :
    t.name === "io" && field === "serve" ? newfn([tstring, newfn([trequest, tresponseinit]), tvoid]) :
    t.name === "io" && field === "fetch" ? newfn([tstring, marks(trequestinit, "?"), tresponse]) :
    t.fields?.find(f => f.code === field).type ||
    (() => { throw new TypeError(`No '${field}' in '${t.name}${t.types.length ? `[${t.types}]` : ""}' type`) })()
  const unify = (a, b) => {
    a = prune(a)
    b = prune(b)
    if (a === b) {
      // nothing
    } else if (a.var) {
      a.instance = b
      if (a.props) {
        if (b.var) {
          b.props = {...(b.props||={}), ...a.props}
        } else {
          Object.keys(a.props).map(name => unify(a.props[name], method(b, name)))
        }
      }
    } else if (b.var) {
      unify(b, a)
    } else {
      if (b.struct && !a.struct) {
        [a, b] = [b, a]
      }
      if (a.struct && !b.struct && b.fields) {
        if (!b.fields.every(bf => a.fields.find(af => bf.code === af.code && (unify(bf.type, af.type), true)))) {
          throw new TypeError(`Struct ${a.struct}(${JSON.stringify(a.fields)}) != ${JSON.stringify(b.fields)}`)
        }
        b.instance = a
        return
      }
      if (a.name !== b.name) {
        throw new TypeError(`Type name ${a.name} != ${b.name}`)
      }
      if (a.types.length !== b.types.length) {
        throw new TypeError(`Type args ${JSON.stringify(a.types)} != ${JSON.stringify(b.types)}`)
      }
      a.types.map((x, i) => unify(x, b.types[i]))
    }
  }
  const safeUnify = (a, b) => {
    try {
      unify(a, b)
      return true
    } catch (e) {
      if (e instanceof TypeError) {
        return false
      } else {
        throw e
      }
    }
  }
  const apply = (f, args) => {
    if (f.var) {
      return (f.instance = newfn([...args, newvar()])).types.at(-1)
    }
    let i = 0
    for (const fa of f.types.slice(0, -1)) {
      if (fa.mark?.includes("*")) {
        while (args[i]) {
          if (!safeUnify(fa.types[0], args[i++])) {
            i--
            break
          }
        }
      } else if (fa.mark?.includes("?")) {
        if (args[i]) {
          safeUnify(fa, args[i++])
        }
      } else if (args[i]) {
        unify(fa, args[i++])
      } else {
        throw new TypeError(`No argument ${JSON.stringify(fa)} at ${i} of ${JSON.stringify(f.types)}`)
      }
    }
    if (i !== args.length) {
      throw new TypeError(`Remain ${JSON.stringify(args.slice(i))}} of ${JSON.stringify(f.types)}`)
    }
    return f.types.at(-1)
  }
  const fresh = (type, nonGeneric) => {
    const d = {}
    const rec = t => {
      const u = prune(t)
      return u.var ?
        (nonGeneric.includes(u.name) ? u : d[u.name] ||= marks(newvar(), u.mark)) :
        (v => (v.types.length && type.generics.push(v.types), v))({...u, types: u.types.map(rec)})
    }
    type.generics ||= []
    return rec(type)
  }
  const returnType = a => Array.isArray(a) ? (a.find(x => x[0]?.code === "return")?.[1]?.type || a.reduce((acc, x) => acc || returnType(x), null)) : null
  const infWith = (top, env, nonGeneric) => {
    const inf = node => node.type ||= _inf(node)
    const _inf = node => {
      if (Array.isArray(node)) {
        const head = node[0]
        const tail = node.slice(1)
        if (Array.isArray(head)) {
          return apply(inf(head), tail.map(inf))
        } else {
          const s = head.code
          if (s === "do") {
            tail.map(inf)
            return returnType(tail) || tail.at(-1).type
          } else if (s === "struct") {
            const [t, generics, ...ts] = tail
            const gargs = generics.map((t, i) => [t.code, t.type = newvar()])
            const args = range(ts.length / 2).map(i => (ts[i*2].type = infWith(ts[i*2+1], {...env, ...Object.fromEntries(gargs)}, nonGeneric.concat(gargs.map(a => a[1].name))), ts[i*2]))
            return env[t.code] = newstruct(t.code, args.map(a => a.type).concat(newnew(args)))
          } else if (s === "def") {
            const args = tail.slice(1, -1).map((t, i) => [t.code, t.type = marks(newvar(), t.mark)])
            const body = infWith(tail.at(-1), {...env, ...Object.fromEntries(args)}, nonGeneric.concat(args.map(a => a[1].name)))
            return env[tail[0].code] = newfn([...args.map(a => a[1]), body])
          } else if (s === "fn") {
            const targs = tail.slice(0, -1).map(_ => newvar())
            const args = tail.slice(0, -1).map((t, i) => [t.code, t.type = marks(targs[i], t.mark)])
            const body = infWith(tail.at(-1), {...env, ...Object.fromEntries(args)}, nonGeneric.concat(targs.map(t => t.name)))
            return newfn([...args.map(a => a[1]), body])
          } else if (s === "for") {
            tail[0].type = tint
            tail.slice(1, 1).map(node => unify(tint, inf(node)))
            infWith(tail.at(-1), {...env, ...{[tail[0].code]: tint}}, nonGeneric)
            return tvoid
          } else if (s === "each") {
            tail[0].type = newvar()
            tail[1].type = newvar()
            tail.slice(2, -1).map(inf)
            infWith(tail.at(-1), {...env, ...{[tail[0].code]: tail[0].type, [tail[1].code]: tail[1].type}}, nonGeneric)
            return tvoid
          } else if (s === "map") {
            const keys = range(tail.length / 2).map(i => inf(tail[i*2]))
            const values = range(tail.length / 2).map(i => inf(tail[i*2+1]))
            return newmap(keys[0] || newvar(), values[0] || newvar())
          } else if ("+-*/%~^=<>!".includes(s[0])) {
            const t = same(tail.map(inf))
            return "== != < > >= <=".split(" ").includes(s) ? tbool : t
          } else if (s === ".") {
            const target = inf(tail[0])
            if (target.name === "tuple") {
              return target.types[tail[1].code]
            } else {
              tail.slice(2).map(inf)
              return method(target, tail[1].code)
            }
          } else if (s === "let" || s === "var") {
            return env[tail[0].code] = inf(tail[1])
          } else if (s === "tuple") {
            return newtuple(tail.map(inf))
          } else if (s === "new") {
            return newnew(range(tail.length / 2).map(i => (tail[i*2].type = inf(tail[i*2+1]), tail[i*2])))
          } else if (s === "vec") {
            return newvec(same(tail.map(inf)))
          } else if (s === "set") {
            return newset(same(tail.map(inf)))
          } else if (s === "||" || s === "&&") {
            return unify(tbool, same(tail.map(inf)))
          } else {
            const argv = tail.map(inf)
            const key = safeJson({code: head.code, argv})
            const ret = apply(inf(head), argv)
            const cache = env.cache ||= {}
            if (key in cache) {
              unify(ret, cache[key])
            } else {
              cache[key] = ret
            }
            return ret
          }
        }
      } else {
        const s = node.code
        return /^[0-9]+\./.test(s)      ? tfloat :
          /^[0-9]+/.test(s)             ? tint :
          s === "true" || s === "false" ? tbool :
          s.startsWith('"')             ? tstring :
          s in env ? fresh(env[s], nonGeneric) :
          (() => { throw new Error(`Type literal '${s}'`) })()
      }
    }
    const same = ([t, ...xs]) => t ? (xs.map(x => unify(t, x)), t) : newvar()
    return inf(top)
  }
  const safeJson = o => {
    const seen = new Set()
    return JSON.stringify(o, (key, value) =>
      typeof value === "object" && value !== null ?
      (seen.has(value) ? seen.size : (seen.add(value), value)) :
      value)
  }

  const copy = t => ({...prune(t), mark: t.mark})
  const fix = node => {
    if (Array.isArray(node)) {
      node.map(fix)
    }
    if (node.type) {
      node.type = copy(node.type)
      node.type.types = node.type.types.map(copy)
      if (node.type.generics) {
        const d = {}
        for (const a of node.type.generics) {
          const t = a.map(prune)
          d[safeJson(t)] ||= t
        }
        node.type.generics = Object.values(d)
      }
    }
  }

  root.map(node => infWith(node, env, [tv1.name]))
  root.map(fix)
  return root
}

export {infer, TypeError}
