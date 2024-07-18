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

const compile = root => {
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
  const tojs = (node) => {
    if (Array.isArray(node)) {
      const [head,...tail] = node
      if (tail.length) {
        if (head.code === '.') {
          if (static.has(tail.map(t => t.code).join(' '))) {
            return `__moa.${tail[0].code}_${tail[1].code}`
          } else {
            const type = tail[0].type
            const target = tojs(tail[0])
            const method = tail[1].code
            return `__moa.${type}_${method}(${target})`
          }
        } else if (head.code === 'iif') {
          return '(' + iifjs(tail.map(tojs)) + ')'
        } else if (head.code === 'if') {
          const cond = tojs(tail.slice(0, -1))
          const body = tojs(tail.at(-1))
          return `if (${cond}) {\n  ${body}\n}`
        } else if (head.code === 'else') {
          if (tail[0].code === 'if') {
            return `else ${tojs(tail)}`
          } else {
            const body = tojs(tail.at(-1))
            return `else {\n  ${body}\n}`
          }
        } else if (head.code === 'switch') {
          const target = tojs(tail.slice(0, -1))
          const name = tail[0].type
          const switchjs = a =>
            a.length === 2 ? `__s.__tag === "${name}.${a[0].code}" ? ${tojs(a[1])} :` :
            a.length === 3 ? `__s.__tag === "${name}.${a[0].code}" ? (${a[1].code} => ${tojs(a[2])})(__s.__val) :` :
            fail(`Unknown switch ${str(tail)}`)
          const body = tail.at(-1).map(switchjs).join('\n')
          return `(__s => ${body} moa.throw("switch", __s))(${target})`
        } else if (head.code === 'for') {
          const a = tail[0].code
          const b = tail[1]?.code
          const c = tail[2]?.code
          const d = tail[3]?.code
          const body = tojs(tail.at(-1))
          return tail.length === 3 ? `for (let ${a}=0; ${a}<${b}; ++${a}) {\n  ${body}\n}` :
            tail.length === 4 ? `for (let ${a}=${b}; ${a}<${c}; ++${a}) {\n  ${body}\n}` :
            tail.length === 5 ? `for (let ${a}=${b}; ${a}<${c}; ${a}+=${d}) {\n  ${body}\n}` :
            fail(`Unknown for ${str(tail)}`)
        } else if (head.code === 'while') {
          const cond = tojs(tail.slice(0, -1))
          const body = tojs(tail.at(-1))
          return `while (${cond}) {${body}}`
        } else if (head.code === 'let') {
          return `const ${tail[0].code} = ${tojs(tail.slice(1))}`
        } else if (head.code === 'var') {
          return `let ${tail[0].code} = ${tojs(tail.slice(1))}`
        } else if (head.code === 'def') {
          const name = tail[0].code
          const args = tail.slice(1).map(x => x.code).join(', ')
          const lines = tail.at(-1).map(tojs)
          const body = [...lines.slice(0, -1), 'return ' + lines.at(-1)].join('\n  ')
          return `function ${name}(${args}) {\n  ${body}\n}`
        } else if (head.code === 'class') {
          const name = tail[0].code
          const fields = tail.at(-1).map(x => x[0].code).join(', ')
          return `function ${name}(${fields}) { return {${fields}} }`
        } else if (head.code === 'enum') {
          const name = tail[0].code
          const enumjs = a => a.length === 1 ? `const ${a[0].code} = {__tag: "${name}.${a[0].code}"}` :
            a.length === 2 && Array.isArray(a[1]) ? `function ${a[0].code}(${a[1].map(x => x[0].code).join(', ')}) { return {__tag: "${name}.${a[0].code}", __val: {${a[1].map(x => x[0].code).join(', ')}}} }` :
            a.length === 2 ? `function ${a[0].code}(__val) { return {__tag: "${name}.${a[0].code}", __val} }` :
            fail(`Unknown enum ${str(name)} with ${str(a)}`)
          return tail.at(-1).map(enumjs).join('\n')
        } else if (head.code === 'catch') {
          const target = tojs(tail[0])
          const handle = tojs(tail[1])
          return `(() => { try { return ${target} } catch (e) { return ${handle}(moa.error(e)) } })()`
        } else if (head.code === '__block') {
          return '\n  ' + tail.map(tojs).join('\n  ') + '\n'
        } else if (head.code === 'dec' || head.code === 'interface' || head.code === 'extern') {
          return ''
        } else if (op1.has(head.code)) {
          return `(${head.code}${tojs(tail)})`
        } else if (op2.has(head.code)) {
          return `(${tojs(tail[0])} ${head.code} ${tojs(tail[1])})`
        } else if (tail.length === 1 && tail[0].length === 0) {
          return tojs(head) + '()'
        } else {
          return `${tojs(head)}(${tail.map(tojs).join(' ')})`
        }
      } else {
        return tojs(head)
      }
    } else {
      return embedded.has(node.code) ?  `__moa.${node.code}` : node.code
    }
  }
  return tojs(root)
}

module.exports = { compile }

if (require.main === module) {
  const parse = src => {
    const line = src => {
      const decompose = s => {
        let [code, type] = s.split('@')
        return {code, type}
      }
      const tokens = src.split(/([()\[\]:;]|(?=[ \n])\.(?= )|\s+)/).filter(x => x.trim()).map(decompose)
      let pos = 0
      const check = f => pos < tokens.length && f(tokens[pos].code)
      const list = a => (t => t.code === ')' ? a : list(a.concat([t])))(unit())
      const bracket = a => (t => t.code === ']' ? a : bracket(a.concat([t])))(unit())
      const suffix = t => check(s => s === '[') && ++pos ? bracket([t]) : t
      const unit = () => (t =>
        t.code === '(' ? list([]) :
        t.code === ':' ? block(top()) :
        suffix(t))(tokens[pos++])
      const block = x => check(s => s === ';') ? _block([x]) : x
      const _block = a => check(s => s === ';') ? (++pos, _block(a.concat([top()]))) : [{code: '__block'}, ...a]
      const top = () => simplify(until(() => check(s => s !== ')' && s !== ';'), unit))
      const simplify = a => a.length === 1 ? a[0] : a
      return block(top())
    }
    return src.split('\n').map(line).flat(1)
  }
  const check = (expect, src) => {
    try {
      const actual = compile(parse(src))
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
  check('while (a) {\n  b\n  c\n}', 'while a: b; c')
  return
  const checkOp1 = (x, ops) => ops.split(' ').map(op => check(`(${op}${x})`, `${op} ${x}`))
  const checkOp2 = (l, r, ops) => ops.split(' ').map(op => check(`(${l} ${op} ${r})`, `${op} ${l} ${r}`))
  const checkOp2Ex = (l, r, ops) => Object.entries(ops).map(([k, v]) => check(`(${l} ${v} ${r})`, `${k} ${l} ${r}`))
  const checkEmbedded = names => names.split(' ').map(name => {
    check(`__moa.${name}`, `${name}`)
    check(`__moa.${name}()`, `(${name})()`)
    check(`__moa.${name}(1)`, `${name} 1`)
    check(`__moa.${name}(1 a)`, `${name} 1 a`)
  })

  // literal
  check('true', 'true')
  check('false', 'false')
  check('1', '1')
  check('1.1', '1.1')
  check('"a"', '"a"')
  check('id', 'id')

  // embedded
  checkEmbedded('bool int float string i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 tuple list set dict log assert')

  // operator
  checkOp2('1', '2', '|| && + - * ** / % & | ^ << >> == != < <= > >=')

  // function
  check('f(1)', 'f 1')
  check('__moa.int(1)', 'int 1')

  // static
  check('__moa.float_inf', '. float inf')
  check('__moa.float_nan', '. float nan')

  // property
  check('__moa.string_size(s)', '. s@string size')

  // method
  check('__moa.int_char(97)()', '(. 97@int char)()')

  // iif
  check('(1 ? 2 : 3)', 'iif 1 2 3')
  check('(1 ? 2 : 3 ? 4 : 5)', 'iif 1 2 3 4 5')

  // if
  check('if (a) {\n  b\n}', 'if a: b')

  // else
  check('if (a) {\n  b\n}\nelse {\n  c\n}', 'if a: b\nelse: c')
  check('if (a) {\n  b\n}\nelse if (c) {\n  d\n}\nelse {\n  e\n}', 'if a: b\nelse if c: d\nelse: e')

  // switch
  check('(__s => __s.__tag === "t.b" ? c : moa.throw("switch", __s))(a)', 'switch a@t: b: c')
  check('(__s => __s.__tag === "t.b" ? (c => c)(__s.__val) : moa.throw("switch", __s))(a)', 'switch a@t: b c: c')

  // throw
  check('__moa.throw(a)', 'throw a')

  // catch
  check('(() => { try { return f(1) } catch (e) { return g(moa.error(e)) } })()', 'catch (f 1) g')

  // return
  check('return(a)', 'return a')

  // for
  check('for (let i=0; i<4; ++i) {\n  a\n}',      'for i 4: a')
  check('for (let i=0; i<4; ++i) {\n  a\n  b\n}', 'for i 4: a; b')
  check('for (let i=1; i<4; ++i) {\n  a\n}',      'for i 1 4: a')
  check('for (let i=1; i<4; i+=2) {\n  a\n}',     'for i 1 4 2: a')

  // while
  check('while (a) {\n  b\n}',      'while a: b')
  check('while (a) {\n  b\n  c\n}', 'while a: b; c')

  // continue
  check('continue', 'continue')

  // break
  check('break', 'break')

  // let
  check('const a = 1', 'let a 1')

  // var
  check('let a = 1', 'var a 1')

  // def
  check('function f() {\n  return 1\n}', 'def f: 1')
  check('function f() {\n  1\n  return 2\n}', 'def f: 1; 2')

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

  print('ok')
}
