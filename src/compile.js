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

const compile = nodes => {
  const op1 = new Set('! ~'.split(' '))
  const op2 = new Set('|| && + - * ** / %  & | ^ ~ << >> != == < <= >= > = '.split(' '))
  const embedded = new Set('bool int float string i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 tuple list set dict log assert throw'.split(' '))
  const static = new Set([
    'float inf',
    'float nan',
  ])
  const iifjs = xs =>
    xs.length === 0 ? fail(`invalid the number of arguments of iif`) :
    xs.length === 1 ? xs[0] :
    `${xs[0]} ? ${xs[1]} : ${iifjs(xs.slice(2))}`
  const tojs = (node) => {
    if (Array.isArray(node)) {
      const [head,...tail] = node
      if (tail.length) {
        if (op1.has(head.code)) {
          return `(${head.code}${tojs(tail)})`
        } else if (op2.has(head.code)) {
          return `(${tojs(tail[0])} ${head.code} ${tojs(tail[1])})`
        } else if (head.code === '.') {
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
  return nodes.map(node => tojs(node)).join('\n')
}

const testCompile = () => {
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
  check('f()', 'f()')

  // embedded
  checkEmbedded('bool int float string i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 tuple list set dict log assert')

  // operator
  checkOp2('1', '2', '|| && + - * ** / % & | ^ << >> == != < <= > >=')

  // function
  check('f(1)', 'f 1')
  check('__moa.int(1)', 'int 1')

  // static
  check('__moa.float_inf', '. float inf')            // TODO: optimize to Infinity
  check('__moa.float_nan', '. float nan')            // TODO: optimize to NaN

  // property
  check('__moa.string_size(s)', '. s@string size')   // TODO: optimize to s.length

  // method
  check('__moa.int_char(97)()', '(. 97@int char)()') // TODO: optimize to String.fromCharCode(97)

  // iif
  check('(1 ? 2 : 3)', 'iif 1 2 3')
  check('(1 ? 2 : 3 ? 4 : 5)', 'iif 1 2 3 4 5')

  // if
  check('if (a) {\n  b\n}', 'if a: b')

  // else
  check('if (a) {\n  b\n}\nelse {\n  c\n}', 'if a: b\nelse: c')
  check('if (a) {\n  b\n}\nelse if (c) {\n  d\n}\nelse {\n  e\n}', 'if a: b\nelse if c: d\nelse: e')

  // switch
  // TODO: impl

  // throw
  check('__moa.throw(a)', 'throw a')

  // catch
  // TODO: impl

  // return
  check('return', 'return')
  check('return(a)', 'return a')

  // for
  // TODO: impl

  // while
  // TODO: impl

  // continue
  check('continue', 'continue')

  // break
  check('break', 'break')

  // let
  // var
  // def
  // dec
  // class
  // enum
  // interface
  // extern
  print('ok')
}

testCompile()
