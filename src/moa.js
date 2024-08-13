#!node
'use strict'

const { readFileSync } = require('fs')
const path = require('node:path')

class TypeError extends Error {}
const log = o => { console.dir(o, {depth: null}); return o }
const str = o => JSON.stringify(o, null, '  ')
const fail = (m, ...a) => { const e = new Error(m); a && (e.detail = a); throw e }
const failUnify = (m, ...a) => { const e = new TypeError(m); a && (e.detail = a); throw e }
const runtimeJs = (function() {'use strict'
const ___string = o => typeof o === 'string' ? o :
  o instanceof Array ? `(list ${o.map(___string).join(' ')})` :
  o instanceof Map ? `(dict ${[...o].map(___string).join(' ')})` :
  o instanceof Set ? `(dict ${[...o].map(___string).join(' ')})` :
  o.toString()
const ___throw = (m, d) => { const e = new Error(m); e.detail = d; throw e }
const ___dict_set = (m, k, v) => (m.set(k, v), v)
const ___assert = (a, b) => ___string(a) === ___string(b) || ___throw(`Assert failure: \`${___string(a)}\` is not \`${___string(b)}\``, a, b)
const ___tuple = (...a) => a
const ___list = (...a) => a
const ___dict = (...a) => new Map([...Array(a.length/2)].map((_,i) => [a[i*2], a[i*2+1]]))
const ___log = (...a) => (console.log(...a.map(___string)), a.at(-1))
}).toString().slice(12, -1)

function main(command, args) {
  if (command === 'to' && args[0] === 'js') {
    const src = readFileSync('/dev/stdin', 'utf-8')
    const ast = infer(parse(src))
    const js = runtimeJs + '\n// ---\n\n' + compileToJs(ast)
    console.log(js)
  } else {
    console.log(`Usage:
      moa                       # launch interactive shell
      moa env [+/-] [<version>] # list versions; use, install or remove a version
      moa ide [<port>]          # launch web IDE
      moa to [<language>]       # compile to a programming language`)
  }
}

function parse(source) {
  // operator | symbols | string | number | id | white spaces
  const regexp = /(\.\.\.[A-Za-z_]*|[!~+\-*/%<>:!=^|&]+|[()\[\]{}]|""".*?"""|"[^]*?(?<!\\)"|-?[0-9]+[0-9_]*(?:\.[0-9_]+)|[0-9A-Za-z_]+|(?:#[^\n]*|[ \n])+)/
  let offset = 0
  const tokens = source.trim().split(regexp).flatMap(code => code.length ? [{code, offset: offset+=code.length}] : [])
  let pos = 0
  const read = () => (t => t.code.match(/^[ #]/) ? tokens[++pos] || '' : t)(tokens[pos] || {code:''})
  const loop = (a, f) => pos < tokens.length ? (v => v ? loop(a.concat([v]), f) : a)(f(read())) :a
  const many = f => loop([], f)
  const until = s => many(t => (t.code.includes('\n') && ++pos, read().code === s ? (++pos, null) : parse_exp()))
  const consume = () => (t => ++pos && t)(read() || fail('out of index', pos, tokens))
  const indent = t => t.code.includes('\n') ? t.code.split('\n').at(-1).length : fail('not break line', t)
  const squash = a => a.length === 1 ? a[0] : a
  const block = a => a.length === 1 ? a[0] : a.length > 1 ? [{code: '__block'}, ...a] : a
  const parse_unit = () => {
    const suffix = t => {
      const close = tokens[pos] || {code: ''}
      const next = read()
      return close.code === '('  ? ++pos && suffix([t, ...until(')')]) :
             close.code === '['  ? ++pos && suffix([close, t, ...until(']')]) :
//             next.code  === ','  ? suffix([t, ...many(t => t.code === ',' && ++pos && consume())]) :
             next.code  === '.'  ? ++pos && suffix([next, t, consume()]) :
//             next.code  === '=>' ? ++pos && [next, t, parse_block()] :
             t
    }
    const t = consume()
    return suffix(
      t.code === '!' ? [t, parse_unit()] :
      t.code === '-' ? [t, parse_unit()] :
      t.code === '[' ? [{...t, code:'list'}, ...until(']')] :
      t.code === '(' ? squash(until(')')) :
      t.code === ':' ? [t].concat([parse_block()]) :
      t.code.startsWith('"""') ? {...t, code: '"' + t.code.slice(3, -3).replaceAll(/"/g, '\\"') + '"'} :
      t.code.match(/^[0-9]+[0-9.]*[xobe_]/) ? {...t, code: Number(t.code.replaceAll(/_/g, '')).toString()} :
      t)
  }
  const parse_exp = () => {
    const lhs = parse_unit()
    const is_op2 = s => typeof s === 'string' && s.match(/^:?[!+\-*/%<>!=^~|&]/) && !'! ~ :'.split(' ').includes(s)
    const op2s = '* ** / // % + ++ - >> << ^ & | < <= > >= == != === !== && || = := += -= *= /= %= **= =>'.split(' ')
    const priority = op => op2s.findIndex(x => x === op)
    const sort = (op, lhs, rhs) =>
      Array.isArray(rhs) && is_op2(rhs[0].code) && priority(op.code) < priority(rhs[0].code) ? [rhs[0], [op, lhs, rhs[1]], rhs[2]] :
      [op, lhs, rhs]
    return is_op2(read().code) ? (op => sort(op, lhs, parse_exp()))(consume()) : lhs
  }
  const parse_block = () => (t => t.code.includes('\n') ? parse_lines(indent(t)) : parse_line())(read())
  const parse_line = () => {
    const is_stop = s => s.includes('\n') || s === ')' || s === ']' || s === ';'
    const a = many(t => !is_stop(t.code) && parse_exp())
    const remain = []
    while (read().code === ";" && ++pos) {
      remain.push(squash(many(t => !is_stop(t.code) && parse_exp())))
    }
    return remain.length ? block([squash(a), ...remain]) : a.length === 0 ? null : a.length === 1 ? a[0] : a
  }
  const parse_lines = n => block(many(t => (t.code.includes('\n') && indent(t) === n && ++pos, parse_line())))
  return parse_lines(0)
}

function infer(root) {
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

function compileToJs(root) {
  const prefix = '___'
  const op1 = new Set('! ~'.split(' '))
  const op2 = new Set('|| && + - * ** / %  & | ^ << >> != == < <= >= > = '.split(' '))
  const embedded = new Set('bool int float string i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 tuple list set dict log assert throw'.split(' '))
  const global = new Set([
    'float inf',
    'float nan',
  ])
  const iifjs = a =>
    a.length === 0 ? fail(`invalid the number of arguments of iif`) :
    a.length === 1 ? a[0] :
    `${a[0]} ? ${a[1]} : ${iifjs(a.slice(2))}`
  const lines = x => !Array.isArray(x) ? [[x]] :
    x[0]?.code === ':' ? lines(x[1]) :
    x[0]?.code === '__block' ? x.slice(1).map(x => Array.isArray(x) ? x : [x]) :
    [x]
  const tojs = (node) => {
    if (Array.isArray(node)) {
      const [head,...tail] = node
      if (head.code === '.') {
        if (global.has(tail.map(t => t.code).join(' '))) {
          return `${prefix}${tail[0].code}_${tail[1].code}`
        } else {
          const type = tail[0].type
          const target = tojs(tail[0])
          const method = tail[1].code
          return `${prefix}${type}_${method}(${target})`
        }
      } else if (head.code === ':') {
        return tojs(tail[0])
      } else if (head.code === 'iif') {
        return '(' + iifjs(tail.map(tojs)) + ')'
      } else if (head.code === 'if') {
        const cond = tojs(tail.length === 2 ? tail[0] : tail.slice(0, -1))
        const body = tojs(tail.at(-1))
        return `if (${cond}) {${body}}`
      } else if (head.code === 'else') {
        if (tail[0].code === 'if') {
          return `else ${tojs(tail)}`
        } else {
          const body = tojs(tail.at(-1))
          return `else {${body}}`
        }
      } else if (head.code === 'switch') {
        const cond = tojs(tail.length === 2 ? tail[0] : tail.slice(0, -1))
        const name = tail[0].type
        const switchjs = a =>
          a.length === 2 ? `__s.__tag === "${name}.${a[0].code}" ? ${tojs(a[1])} :` :
          a.length === 3 ? `__s.__tag === "${name}.${a[0].code}" ? (${a[1].code} => ${tojs(a[2])})(__s.__val) :` :
          fail(`Unknown switch ${str(a)}`)
        const x = tail.at(-1)[1]
        const body = (Array.isArray(x[0]) ? x : [x]).map(switchjs).join('\n')
        return `(__s => ${body} moa.throw("switch", __s))(${cond})`
      } else if (head.code === 'for') {
        const a = tail[0].code
        const b = tail[1]?.code
        const c = tail[2]?.code
        const d = tail[3]?.code
        const body = tojs(tail.at(-1))
        return tail.length === 3 ? `for (let ${a}=0; ${a}<${b}; ++${a}) {${body}}` :
          tail.length === 4 ? `for (let ${a}=${b}; ${a}<${c}; ++${a}) {${body}}` :
          tail.length === 5 ? `for (let ${a}=${b}; ${a}<${c}; ${a}+=${d}) {${body}}` :
          fail(`Unknown for ${str(tail)}`)
      } else if (head.code === 'while') {
        const cond = tojs(tail.length === 2 ? tail[0] : tail.slice(0, -1))
        const body = tojs(tail.at(-1))
        return `while (${cond}) {${body}}`
      } else if (head.code === 'let') {
        const value = tojs(tail.length === 2 ? tail[1] : tail.slice(1, -1))
        return `const ${tail[0].code} = ${value}`
      } else if (head.code === 'var') {
        const value = tojs(tail.length === 2 ? tail[1] : tail.slice(1, -1))
        return `let ${tail[0].code} = ${value}`
      } else if (head.code === 'def') {
        const name = tail[0].code
        const args = tail.slice(1, -1).map(x => x.code).join(', ')
        const last = tail.at(-1)[0]?.code === ':' ? tail.at(-1)[1] : tail.at(-1)
        const steps = (last[0]?.code === '__block' ? last.slice(1) : [last]).map(tojs)
        const body = [...steps.slice(0, -1), 'return ' + steps.at(-1)].join('\n')
        return `function ${name}(${args}) {${body}}`
      } else if (head.code === 'class') {
        const name = tail[0].code
        const fields = lines(tail.at(-1)).map(x => x[0].code).join(', ')
        return `function ${name}(${fields}) { return {${fields}} }`
      } else if (head.code === 'enum') {
        const name = tail[0].code
        const enumjs = a => a.length === 1 ? `const ${a[0].code} = {__tag: "${name}.${a[0].code}"}` :
          a.length === 2 && Array.isArray(a[1]) ?
          (a1 => `function ${a[0].code}(${a1.map(x => x[0].code).join(', ')}) { return {__tag: "${name}.${a[0].code}", __val: {${a1.map(x => x[0].code).join(', ')}}} }`)(lines(a[1])) :
          a.length === 2 ? `function ${a[0].code}(__val) { return {__tag: "${name}.${a[0].code}", __val} }` :
          fail(`Unknown enum ${str(name)} with ${str(a)}`)
        return lines(tail.at(-1)).map(enumjs).join('\n')
      } else if (head.code === 'catch') {
        const target = tojs(tail[0])
        const handle = tojs(tail[1])
        return `(() => { try { return ${target} } catch (e) { return ${handle}(${prefix}error(e)) } })()`
      } else if (head.code === '__block') {
        return tail.map(tojs).join('\n')
      } else if (head.code === 'dec' || head.code === 'interface' || head.code === 'extern') {
        return ''
      } else if (head.code === '-' && tail.length === 1) {
        return `(-${tojs(tail[0])})`
      } else if (op1.has(head.code)) {
        return `(${head.code}${tojs(tail[0])})`
      } else if (op2.has(head.code)) {
        return `(${tojs(tail[0])} ${head.code} ${tojs(tail[1])})`
      } else if (tail.length === 1 && tail[0].length === 0) {
        return tojs(head) + '()'
      } else {
        return `${tojs(head)}(${tail.map(tojs).join(', ')})`
      }
    } else {
      return embedded.has(node.code) ?  `${prefix}${node.code}` : node.code
    }
  }
  try {
    return tojs(root)
  } catch (e) {
    e.detail = root
    throw e
  }
}

module.exports = { main, parse, infer, runtimeJs, compileToJs, TypeError }

if (require.main === module) {
  main(process.argv[2], process.argv.slice(3))
}
