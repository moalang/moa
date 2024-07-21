const log = o => { console.dir(o, {depth: null}); return o }
const str = o => JSON.stringify(o, null, '  ')
const fail = (m, ...a) => { const e = new Error(m); a && (e.detail = a); throw e }

const compile = root => {
  const prefix = '___'
  const op1 = new Set('! ~'.split(' '))
  const op2 = new Set('|| && + - * ** / %  & | ^ ~ << >> != == < <= >= > = '.split(' '))
  const embedded = new Set('bool int float string i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 tuple list set dict log assert throw'.split(' '))
  const static = new Set([
    'float inf',
    'float nan',
  ])
  const iifjs = a =>
    a.length === 0 ? fail(`invalid the number of arguments of iif`) :
    a.length === 1 ? a[0] :
    `${a[0]} ? ${a[1]} : ${iifjs(a.slice(2))}`
  const nest = x => !Array.isArray(x) ? [[x]] :
    x[0]?.code === ':' ? nest(x[1]) :
    x[0]?.code === '__block' ? x.slice(1).map(x => Array.isArray(x) ? x : [x]) :
    [x]
  const tojs = (node) => {
    if (Array.isArray(node)) {
      const [head,...tail] = node
      if (head.code === '.') {
        if (static.has(tail.map(t => t.code).join(' '))) {
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
        const lines = (last[0]?.code === '__block' ? last.slice(1) : [last]).map(tojs)
        const body = [...lines.slice(0, -1), 'return ' + lines.at(-1)].join('\n')
        return `function ${name}(${args}) {${body}}`
      } else if (head.code === 'class') {
        const name = tail[0].code
        const fields = nest(tail.at(-1)).map(x => x[0].code).join(', ')
        return `function ${name}(${fields}) { return {${fields}} }`
      } else if (head.code === 'enum') {
        const name = tail[0].code
        const enumjs = a => a.length === 1 ? `const ${a[0].code} = {__tag: "${name}.${a[0].code}"}` :
          a.length === 2 && Array.isArray(a[1]) ?
          (a1 => `function ${a[0].code}(${a1.map(x => x[0].code).join(', ')}) { return {__tag: "${name}.${a[0].code}", __val: {${a1.map(x => x[0].code).join(', ')}}} }`)(nest(a[1])) :
          a.length === 2 ? `function ${a[0].code}(__val) { return {__tag: "${name}.${a[0].code}", __val} }` :
          fail(`Unknown enum ${str(name)} with ${str(a)}`)
        return nest(tail.at(-1)).map(enumjs).join('\n')
      } else if (head.code === 'catch') {
        const target = tojs(tail[0])
        const handle = tojs(tail[1])
        return `(() => { try { return ${target} } catch (e) { return ${handle}(${prefix}error(e)) } })()`
      } else if (head.code === '__block') {
        return tail.map(tojs).join('\n')
      } else if (head.code === 'dec' || head.code === 'interface' || head.code === 'extern') {
        return ''
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
  return tojs(root)
}

module.exports = { compile }

if (require.main === module) {
  const { parse } = require('./parse.js')
  const hint = x => Array.isArray(x) ? rec([], x) : x
  const rec = (acc, a) => a.length === 0 ? acc :
    a[1]?.code === '@' ? rec(acc.concat((a[0].type = a[2].code, a[0])), a.slice(3)) :
    rec(acc.concat([ Array.isArray(a[0]) ? hint(a[0]) : a[0] ]), a.slice(1))
  const check = (expect, src) => {
    try {
      const actual = compile(hint(parse(src)))
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

  // literal
  check('true', 'true')
  check('false', 'false')
  check('1', '1')
  check('1.1', '1.1')
  check('"a"', '"a"')
  check('id', 'id')

  // embedded
  for (const name of 'bool int float string i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 tuple list set dict log assert'.split(' ')) {
    check(`___${name}`, `${name}`)
    check(`___${name}()`, `${name}()`)
    check(`___${name}(1)`, `${name} 1`)
    check(`___${name}(1, a)`, `${name} 1 a`)
  }

  // operator
  check('(~1)', '~1')
  check('(!true)', '!true')
  for (const op of '|| && + - * ** / % & | ^ << >> == != < <= > >='.split(' ')) {
    check(`(1 ${op} 2)`, `1${op}2`)
    check(`(1 ${op} 2)`, `1 ${op} 2`)
  }

  // function
  check('f(1)', 'f 1')
  check('___int(1)', 'int 1')

  // static
  check('___float_inf', '. float inf')
  check('___float_nan', '. float nan')

  // property
  check('___string_size(s)', '. s@string size')

  // method
  check('___int_char(97)()', '(. 97@int char)()')

  // iif
  check('(1 ? 2 : 3)', 'iif 1 2 3')
  check('(1 ? 2 : 3 ? 4 : 5)', 'iif 1 2 3 4 5')

  // if
  check('if (a) {b}', 'if a: b')

  // else
  check('if (a) {b}\nelse {c}', 'if a: b\nelse: c')
  check('if (a) {b}\nelse if (c) {d}\nelse {e}', 'if a: b\nelse if c: d\nelse: e')

  // switch
  check('(__s => __s.__tag === "t.b" ? c : moa.throw("switch", __s))(a)', 'switch a@t: b: c')
  check('(__s => __s.__tag === "t.b" ? (c => c)(__s.__val) : moa.throw("switch", __s))(a)', 'switch a@t: b c: c')

  // throw
  check('___throw(a)', 'throw a')

  // catch
  check('(() => { try { return f(1) } catch (e) { return g(___error(e)) } })()', 'catch (f 1) g')

  // return
  check('return(a)', 'return a')

  // for
  check('for (let i=0; i<4; ++i) {a}',      'for i 4: a')
  check('for (let i=0; i<4; ++i) {a\nb}', 'for i 4: a; b')
  check('for (let i=1; i<4; ++i) {a}',      'for i 1 4: a')
  check('for (let i=1; i<4; i+=2) {a}',     'for i 1 4 2: a')

  // while
  check('while (a) {b}',      'while a: b')
  check('while (a) {b\nc}', 'while a: b; c')

  // continue
  check('continue', 'continue')

  // break
  check('break', 'break')

  // let
  check('const a = 1', 'let a 1')

  // var
  check('let a = 1', 'var a 1')

  // def
  check('function f() {return 1}', 'def f: 1')
  check('function f() {1\nreturn 2}', 'def f: 1; 2')

  // class
  check('function a(b) { return {b} }', 'class a: b int')
  check('function a(b, c) { return {b, c} }', 'class a: b int; c string')
  check('function a(b, c) { return {b, c} }', 'class a t: b int; c string')

  // enum
  check('const b = {__tag: "a.b"}', 'enum a: b')
  check('const b = {__tag: "a.b"}\nconst c = {__tag: "a.c"}', 'enum a: b; c')
  check('function b(__val) { return {__tag: "a.b", __val} }', 'enum a: b int')
  check('function b(c) { return {__tag: "a.b", __val: {c}} }', 'enum a: b: c int')

  // dec
  check('', 'dec f a')

  // interface
  check('', 'interface a: b')

  // extern
  check('', 'extern a: b')

  console.log('ok')
}
