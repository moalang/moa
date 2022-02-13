'use strict'

const write = (...a) => process.stdout.write(a.map(x => x.toString()).join(' '))
const puts = (...a) => console.log(...a)
const pp = (...a) => (write('TRACE:'), console.dir(a, {depth: null}), a[a.length - 1])
const fail = (m, ...a) => { a.length && pp(m, ...a); throw new Error(m) }
const showType = target => {
  const show = t => t.instance ? show(t.instance) : (
    t.name && t.types.length >= 1 ? `(${t.name} ${t.types.map(show).join(' ')})` :
    t.name ? t.name : `(${t.types.map(show).join(' ')})`
  ) + (t.tvars.length === 0 ? '' : `[${t.tvars.map(show).join(' ')}]`) + (target.prop ? `.${target.prop}` : '')
  const s = show(target)
  const o = {}
  const r = s.replace(/\d+/g, t => o[t] ||= Object.keys(o).length + 1)
  return r
}
const stdjs = (function() {
const moa_string_at = (o, n) => n < o.length ? o[n]  : fail('Out of index')
const moa_string_contains = (o, s) => o.includes(s)
const moa_string_sub = (o, a, b) => o.replaceAll(a, b)
const moa_string_rsub = (o, a, b) => o.replaceAll(new RegExp(a, 'g'), b)
const moa_string_rsplit = (o, r) => o.split(new RegExp(r, 'g'))
const moa_array_at = (o, n) => n < o.length ? o[n]  : fail('Out of index')
const moa_array_append = (o, a) => o.concat(a)
const moa_array_contains = (o, s) => o.includes(s)
const moa_array_keep = (o, f) => o.filter(f)
}).toString().slice(12, -1).trim()

function compile(src) {
  let pos = 0
  const newToken = s => Object.assign(new String(s.replace(/^ +/, '')), {pos: (pos+=s.length) - s.length})
  const tokens = src.split(/(\(\)|[():\[\],]|[\+\-\*\/%&|!=><\.]+|"[^"]*?"|`[^`]*?`|[ \n]+|[a-zA-Z0-9_]+)/).map(newToken).filter(x => x.length)
  if (pos !== src.length) { fail(`Failed to tokenize ${pos} !== ${src.length}`) }
  const is_op2 = x => '+-*/%|&!=<>.,'.includes(x[0])
  const parse = () => {
    let pos = 0
    const next = o => { ++pos; return o }
    const until = (f, g) => {
      const a = []
      while (pos < tokens.length && tokens[pos][0] != '\n' && f(tokens[pos])) {
        a.push((g || consume)())
      }
      return a
    }
    const reads = f => until(f || (() => true))
    const sepby = f => [reads()].concat(pos < tokens.length && f(tokens[pos]) ? sepby(next(f)) : [])
    const consume = () => {
      const t = tokens[pos++]
      const unnest = a => Array.isArray(a) && a.length === 1 ? unnest(a[0]) : a
      const array = a => Array.isArray(a) ? a : [a]
      const node = t == '(' ? unnest(next(reads(t => t != ')'))) :
        t == '[' ? [newToken('array')].concat(next(reads(t => t != ']'))) :
        t == ':' && tokens[pos][0] == '\n' ? [newToken('do')].concat(top(tokens[pos++])) :
        t == ':' ? reads() : t
      const combine = node => (t => !t ? node :
        t == ',' ? combine([node].concat(until(t => t == ',' && ++pos, () => tokens[pos++]))) :
        t == '.' ? combine([t, node, next(tokens[++pos])]) :
        t == '=>' ? combine([t, array(node), (++pos, consume())]) :
        is_op2(t) ? combine([tokens[pos++], node, consume()]) :
        t == '()' ? combine((node.suffix = '()', pos++, node)) :
        t == '(' && t.pos === tokens[pos - 1].pos +tokens[pos - 1].length ? ++pos && combine([node].concat(next(reads(t => t != ')')))) : node)(tokens[pos])
      return combine(node)
    }
    const top = br => sepby(t => t.toString() == br)
    return top('\n')
  }
  const gen = node => {
    const exps = a => a.length === 1 ? gen(a[0]) : `(() => {\n  ${a.map((e, i) => (i === a.length - 1 ? 'return ' : '') + gen(e)).join('\n  ')}\n})()`
    const matcher = ([tag, alias, ...exp]) => `v.__tag === '${tag}' ? (${alias} => ${gen(exp)})(v.__value)`
    const branch = a => a.length == 0 ? fail('empty branch') : a.length === 1 ? a[0] : `${a[0]} ? ${a[1]} : ` + branch(a.slice(2))
    const handle = ([a, b]) => `(() => { try { return ${a} } catch (__e) { return (${b})(__e) } })()`
    const methods = {
      string: 'at contains sub rsub rsplit'.split(' '),
      array: 'at append contains keep'.split(' '),
    }
    const method = (o, m, a) => _method(gen(o), gen(m), showType(o.type).split('[')[0], a || [])
    const _method = (o, m, t, a) =>
      t == 'string' && m == 'size' ? `${o}.length` :
      t == 'array' && m == 'size' ? `${o}.length` :
      (methods[t] || []).includes(m) ? `moa_${t}_${m}(${o}, ${a})` :
      `${o}.${m}` + (a.length ? `(${a})` : '')
    const apply = ([head, ...tail]) =>
      tail.length === 0 ? gen(head) :
        head == 'def' ? `const ${tail[0]} = (${tail.slice(1, -1)}) => ${gen(tail[tail.length - 1])}` :
        head == 'struct' ? (keys => `const ${tail[0]} = (${keys}) => ({${keys}})`)(tail[1].slice(1).map(a => a[0])) :
        head == 'adt' ? (tags => `const ${tail[0]} = {${tags.map(tag => `${tag}: __value => ({__tag: '${tag}', __value})`)}}`)(tail[1].slice(1).map(a => a[0])) :
        head == 'match' ? `(v => ${tail[1].slice(1).map(matcher).join(' : ')} : error(\`Does not match \${v}\`))(${gen(tail[0])})` :
        head == 'array' ? `[${tail.map(gen).join(', ')}]` :
        head == 'do' ? exps(tail) :
        head == 'var' ? `let ${tail[0]} = ${gen(tail.slice(1))}` :
        head == 'let' ? `const ${tail[0]} = ${gen(tail.slice(1))}` :
        head == 'if' ? branch(tail.map(gen)) :
        head == 'catch' ? handle(tail.map(gen)) :
        head == 'hint' ? '' :
        head == '=>' ? `((${tail[0] + ') => ' + gen(tail[1])})` :
        head == '.' ? method(tail[0], tail[1]) :
        head == '/' ? `(d => d === 0 ? fail('Zero division error') : ${gen(tail[0])} / d)(${gen(tail[1])})` :
        head == '==' ? `JSON.stringify(${gen(tail[0])}) === JSON.stringify(${gen(tail[1])})` :
        head == '!=' ? `JSON.stringify(${gen(tail[0])}) !== JSON.stringify(${gen(tail[1])})` :
        head == '-' && tail.length === 1 ? '-' + gen(tail[0]) :
        !Array.isArray(head) && is_op2(head) ? gen(tail[0]) + head + gen(tail[1]) :
        Array.isArray(head) && head[0] == '.' ? method(head[1], head[2], tail.map(gen)) :
        gen(head) + '(' + tail.map(gen).join(', ') + ')'
    return (Array.isArray(node) ? apply(node) : node) + (node.suffix || '')
  }
  const infer = nodes => {
    const fresh = (type, nonGeneric) => {
      const d = {}
      const rec = t => {
        const p = prune(t)
        return p.var ?
          (nonGeneric.includes(p.name) ? p : d[p.name] ||= tvar()) :
          Object.assign({}, p, {types: p.types.map(rec)})
      }
      return rec(type)
    }
    const _unify = (a, b, debug) => {
      a = prune(a)
      b = prune(b)
      if (a.var) {
        if (a.name !== b.name) {
          a.instance = b
        }
      } else if (b.var) {
        _unify(b, a, debug)
      } else {
        if (a.name !== b.name || a.types.length !== b.types.length) {
          fail(`type miss match`, showType(a), showType(b), debug)
        }
        a.types.map((t,i) => _unify(t, b.types[i]), debug)
      }
    }

    const prune = t => (t.var && t.instance) ? t.instance = prune(t.instance) : t
    const analyse = (node, env, nonGeneric) => node.type = prune(_analyse(node, env, nonGeneric))
    const _analyse = (node, env, nonGeneric) => {
      const unify = (a, b) => _unify(a, b, node)
      if (Array.isArray(node)) {
        let [head, ...tail] = node
        if (head == '=>') {
          const newEnv = Object.assign({}, env)
          const argv = tail[0].map(arg => newEnv[arg.toString()] = (arg.type ||= tvar()))
          //tail[0].map((arg, i) => arg.type = newEnv[arg.toString()] = argv[i])
          return tlambda(...argv, analyse(tail[1], newEnv, nonGeneric.concat(argv.map(t => t.name))))
        } else if (head == 'var' || head == 'let' || head == '=') {
          const name = tail[0].toString()
          return head.type = env[name] = analyse(tail.slice(1), env, nonGeneric)
        } else if (head == 'def') {
          const name = tail[0].toString()
          const args = tail.slice(1, -1).map(arg => (arg.type = tvar(), arg))
          const body = tail.slice(-1)[0]
          const newEnv = Object.assign({}, env)
          const hints = tenv[name] && tenv[name].types
          if (hints) {
            if (args.length !== hints.length - 1) {
              fail(`wrong number of arguments (given ${args.length}, expected ${hints.length-1})`)
            }
            args.map((a,i) => unify(a.type, hints[i]))
          }
          args.forEach(arg => newEnv[arg.toString()] = arg.type)
          const ret = analyse(body, newEnv, nonGeneric.concat(args.map(t => t.type.name)))
          if (hints) {
            unify(ret, hints[hints.length - 1])
          }
          return head.type = env[name] = tlambda(...args.map(t => t.type), ret)
        } else if (head == 'do') {
          const steps = tail.map(step => analyse(step, env, nonGeneric))
          return head.type = steps[steps.length - 1]
        } else if (head == 'array') {
          const t = tarray(tvar())
          tail.map(x => unify(t.tvars[0], analyse(x, env, nonGeneric)))
          return t
        } else if (head == 'match') {
          const target = analyse(tail[0], env, nonGeneric)
          const tv = tvar()
          for (const [tag, id, exp] of tail[1].slice(1)) {
            unify(tv, analyse(exp, Object.assign({}, env, {[id]: tenv[`${target.name}__${tag}`]}), nonGeneric))
          }
          return tv
        } else if (head == '.') {
          const [target, id] = tail
          const t = analyse(target, env, nonGeneric)
          return tenv[t.name](...t.tvars)[id] || fail(`Not found '${id}' in '${t.name}'`)
        } else if (head == '-' && tail.length === 1) {
          return tint
        } else if (tail.length) {
          const rt = (env.__cache[JSON.stringify(tail)] ||= tvar()) // fix tvar
          const ft = analyse(head, env, nonGeneric)
          ft.types && ft.types.slice(0, -1).map((arg, i) => {
            if (tail[i][0] == '=>') {
              arg.types.slice(0, -1).map((f, j) => {
                unify(f, tail[i][1][j].type ||= tvar())
              })
            }
          })
          const argv = tail.map(t => analyse(t, env, nonGeneric))
          unify(tlambda(...argv, rt), ft)
          return rt
        } else {
          return analyse(head, env, nonGeneric)
        }
      } else {
        const v = node.toString()
        return v.match(/^[0-9]/) ? tint :
          v.match(/^["`]/) ? tstring :
          env[v] ? fresh(env[v], nonGeneric) :
          fail(`Not found '${v}' in env`, ...Object.keys(env).sort())
      }
    }

    let tvarSequence = 0
    const newt = o => (o.name||='', o.types||=[], o.tvars||=[], o)
    const tvar = () => (name => newt({name, var: true}))((++tvarSequence).toString())
    const tlambda = (...types) => types.length === 1 ? types[0] : newt({types})
    const ttype = (name, ...tvars) => newt({name, tvars})
    const tint = ttype('int')
    const tstring = ttype('string')
    const tbool = ttype('bool')
    const tarray = t => ttype('array', t)
    const v1 = tvar()
    const v2 = tvar()
    const tenv = {
      string: () => ({
        size: tint,
        at: tlambda(tint, tstring),
        contains: tlambda(tstring, tbool),
        sub: tlambda(tstring, tstring, tstring),
        rsub: tlambda(tstring, tstring, tstring),
        split: tlambda(tstring, tarray(tstring)),
        rsplit: tlambda(tstring, tarray(tstring)),
      }),
      array: t => ({
        size: tint,
        at: tlambda(tint, t),
        contains: tlambda(t, tbool),
        map: (u => tlambda(tlambda(t, u), tarray(u)))(tvar()),
        keep: tlambda(tlambda(t, tbool), tarray(t)),
        append: tlambda(t, tarray(t)),
      }),
      __tester: t => ({
        eq: tlambda(v1, v1, v2),
      }),
      selfcheck: tlambda(ttype('__tester'), v1),
    }
    const topEnv = {
      __cache: {},
      '=': tlambda(v1, v1),
      if: tlambda(tbool, v1, v1, v1),
      fail: tlambda(v1, v2),
      catch: tlambda(v1, tlambda(v2, v1), v1),
    }
    const define = (s, t) => s.split(' ').map(x => topEnv[x] = t)
    define('true false', tbool)
    define('+ - * / % += -= *= /= %=', tlambda(tint, tint, tint))
    define('< <= > >=', tlambda(tint, tint, tbool))
    define('== !=', tlambda(v1, v1, tbool))
    define('&& ||', tlambda(tbool, tbool, tbool))
    for (const node of nodes.filter(node => node[0] == 'struct')) {
      const name = node[1].toString()
      const fields = node[node.length - 1].slice(1)
      const d = {}
      fields.map(([id, tname]) => d[id] = ttype(tname.toString()))
      tenv[name] = () => d
      topEnv[name] = tlambda(...fields.map(([id, tname]) => ttype(tname.toString())), ttype(name))
    }
    for (const node of nodes.filter(node => node[0] == 'adt')) {
      const name = node[1].toString()
      const d = {}
      for (const [tag, tname] of node[node.length - 1].slice(1)) {
        const t = ttype(tname.toString())
        d[tag] = tlambda(t, ttype(name))
        tenv[`${name}__${tag}`] = t
      }
      tenv[name] = () => d
      topEnv[name] = ttype(name)
    }
    for (const node of nodes.filter(node => node[0] == 'hint')) {
      const name = node[1].toString()
      const types = node.slice(2).map(x => ttype(x.toString()))
      tenv[name] = tlambda(...types)
    }
    return nodes.filter(node => node[0] == 'def').map(node => analyse(node, topEnv, []))
  }
  const nodes = parse()
  infer(nodes)
  const js = stdjs + '\n' + nodes.map(gen).join('\n')
  return {js, nodes}
}

const test = () => {
  const run = js => {
    try {
      return eval(js + '\nmain()')
    } catch (e) {
      return e.message
    }
  }
  const exp = (expected, exp, ...defs) => {
    const src = defs.concat([`def main: ${exp}`]).join('\n')
    const js = compile(src).js
    const actual = run(js)
    if (JSON.stringify(expected) === JSON.stringify(actual)) {
      write('.')
    } else {
      puts('src:', src)
      puts('js:', js)
      puts('expected:', expected)
      puts('actual:', actual)
      process.exit(1)
    }
  }

  // primitives
  exp(1, '1')
  exp('hi', '"hi"')
  exp('hi', '`hi`')
  exp('"', '`"`')
  exp('\n', '`\n`')
  exp('\n', '`\\n`')
  exp([1, 2], '[1 2]')
  exp(1, '(n => n) 1')

  // methods
  exp('i', '"hi".at(1).at(0)')
  exp([2, 3], '[1 2].map(n => n + 1)')
  exp([2, 3], '[1 2 3].map(n => n + 1).keep(n => n <= 3)')
  exp(['A'], '["a"].map(s => s.sub("a" "A"))')

  // int
  exp(-1, '(-1)')
  exp(0, '-1 + 1')
  exp(0, 'add 1 (-1)', 'def add a b: a + b')

  // string
  exp(2, '"hi".size')
  exp('i', '"hi".at 1')
  exp('Out of index', '"hi".at 3')
  exp(['a', 'b'], '"a,b".split ","')
  exp(true, '"hi".contains "h"')
  exp(false, '"hi".contains "z"')
  exp('heo', '"hello".sub "l" ""')

  // string with regular expression
  exp('h_o', '"hello".rsub `[el]+` "_"')
  exp(['1', '+', '2'], '"1 + 2".rsplit(`([0-9\+])`).map(s => s.rsub(` ` "")).keep(s => s != "")')

  // array
  exp(2, '[1 2].size')
  exp([1, 2], '[1].append 2')
  exp([2, 3], '[1 2].map n => n + 1')
  exp([1, 3], '[1 2 3].keep n => (n % 2) == 1')
  exp(true, '[1 2].contains 1')
  exp(false, '[1 2].contains 3')

  // function
  exp(1, 'one()', 'def one: 1')
  exp(3, 'add 1 2', 'def add a b: a + b')
  exp(6, 'calc 2 3', `def calc a b:
  def mul a b: a * b
  mul a b`)
  exp(3, `
  var a 1
  def inc: a += 1
  def twice f:
    f()
    f()
  twice inc
  a`)

  // struct
  exp({x:1, y:2}, 'vector2 1 2', 'struct vector2:\n  x int\n  y int')
  exp(2, '(vector2 1 2).y', 'struct vector2:\n  x int\n  y int')

  // algebraic data type
  exp({__tag: 'a', __value: 1}, 'ab.a 1', 'adt ab:\n  a int\n  b string')
  exp(1, 'match (ab.a 1):\n  a v: v\n  b s: s.size', 'adt ab:\n  a int\n  b string')
  exp(2, 'match (ab.b "hi"):\n  a v: v\n  b s: s.size', 'adt ab:\n  a int\n  b string')

  // exp
  exp(3, '1 + 2')
  exp(7, '1 + 2 * 3')
  exp(5, '1 * 2 + 3')
  exp(true, '([1 2].size == 1 + 1) && [3 4].size == 2')
  exp(true, '(s 1) == (s 1)', 'struct s:\n  value int')
  exp(3, '(a,b => a + b) 1 2')
  exp(true, '1 == 1')
  exp(false, '1 != 1')

  // variable
  exp(1, '\n  var n 0\n  n = 1\n  n')
  exp(3, '\n  var a 1\n  a += 2\n  a')
  exp(3, '\n  var a 1\n  def inc: a += 1\n  inc()\n  inc()\n  a')

  // constant
  exp(2, '\n  let a inc 1\n  a', 'def inc a: a + 1')

  // branch
  exp(1, 'if true 1 2')
  exp(2, 'if false 1 2')
  exp(2, 'if (true && (1 == 2)) 1 2')

  // error handling
  exp('Zero division error', '\n  1/0\n  1')
  exp('error', '\n  fail "error"\n  1')
  exp(1, 'catch(1 _ => 2)')
  exp(2, 'catch((fail "error") e => 2)')

  // do block
  exp(1, 'do 1')
  exp(5, 'do 1 (2 + 3)')

  // type inference
  const type = (expected, exp, ...defs) => {
    const src = defs.concat([`def main: ${exp}`]).join('\n')
    const nodes = compile(src).nodes
    const actual = showType(nodes[nodes.length - 1].type)
    if (JSON.stringify(expected) === JSON.stringify(actual)) {
      write('.')
    } else {
      puts('src:', src)
      puts('expected:', expected)
      puts('actual:', actual)
      process.exit(1)
    }
  }
  // primitives
  type('int', '1')
  type('string', '"hi"')
  type('array[1]', '[]')
  type('array[int]', '[1]')
  type('(1 1)', 'n => n')
  type('int', '(n => n) 1')
  type('int', '(a,b => a) 1 "a"')
  type('string', '(a,b => b) 1 "a"')
  type('int', '(a,b => a + b) 1 2')

  // methods
  type('array[int]', '[1].map(n => n + 1)')
  type('array[string]', '[1].map(n => "a")')
  type('array[int]', '[1].map(n => n + 1).keep(n => n <= 3)')

  // formula
  type('int', '1 + 1')
  type('int', '1 * 1')

  // exp
  type('int', '1 + 1')
  type('bool', '1 > 1')

  // branch
  type('int', 'if true 1 2')
  type('bool', 'if true true true')

  // value
  type('int', '(def value 1)')

  // simple function
  type('(int int)', '(def inc a (+ a 1))')
  type('(int int int)', '(def add a b (+ a b))')

  // generics
  type('(1 1)', 'def f a: a')
  type('(1 2 1)', 'def f a b: a')
  type('(1 2 2)', 'def f a b: b')
  type('int', 'f 1', 'def f a: a')
  type('int', 'if (f true) (f 1) (f 2)', 'def f a: a')

  // combinations
  type('int', '(f 1) + (g 1)', 'def f x: x + 1', 'def g x: x + 2')
  type('((1 2) (2 3) 1 3)', 'def _ f g x: g (f x)')
  type('((1 2 3) (1 2) 1 3)', 'def _ x y z: x z (y z)')
  type('(1 (1 bool) (1 1))', 'def _ b x: if (x b) x (x => b)')
  type('(bool bool)', 'def _ x: if true x (if x true false)')
  type('(bool bool bool)', 'def _ x y: if x x y')
  type('(1 1)', 'def _ n : (x => x (y => y)) (f => f n)')
  type('((1 2) 1 2)', 'def _ x y: x y')
  type('((1 2) ((1 2) 1) 2)', 'def _ x y: x (y x)')
  type('(1 ((1 2 3) 4 2) (1 2 3) 4 3)', 'def _ h t f x: f h (t f x)')
  type('((1 1 2) ((1 1 2) 1) 2)', 'def _ x y: x (y x) (y x)')
  type('(((1 1) 2) 2)', 'def _ y: id (y id)', 'def id x: x')
  type('int', 'def f: if (id true) (id 1) (id 2)', 'def id x: x')
  type('int', 'def g: (f true) + (f 4)', 'def f x: 3')
  type('(bool (1 1))', 'h', 'def f x: x', 'def g y: y', 'def h b: if b (f g) (g f)')

  // type hint
  type('(1 1)', 'f', 'def f x: x')
  type('(string string)', 'f', 'hint f string string', 'def f x: x')
  type('(int int)', 'f', 'hint f int int', 'def f x: x')

  puts('ok')
  return true
}

function bootstrap() {
  let fs = require('fs')
  let moa = fs.readFileSync('moa.moa', 'utf8')
  let prefix = `#!/usr/bin/env node
'use strict'
`
  let suffix = `

let command = process.argv[2]
if (command === 'build') {
  let fs = require('fs')
  let src = fs.readFileSync(process.argv[3], 'utf8')
  console.log(compile(src))
} else if (command === 'version') {
  console.log('moa0.0.1 js')
} else if (command === 'selfcheck') {
  const tester = {
    eq: (a, b) => {
      if (JSON.stringify(a) === JSON.stringify(b)) {
        process.stdout.write('.')
      } else {
        console.log("expect: ", a)
        console.log("actual: ", b)
        process.exit(-1)
      }
    }
  }
  selfcheck(tester)
  console.log('ok')
} else {
  console.log(\`Moa is a tool for managing Moa source code.

Usage:

  moa <command> [arguments]

The commands are:

  build       compile packages and dependencies
  version     print Moa version\`)
}`
  let js = prefix + compile(moa.trim()).js + suffix
  fs.writeFileSync(__dirname + '/../bin/moa', js)
}

test() && bootstrap()
