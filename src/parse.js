'use strict'
// Convert Moa code to internal expression which looks like LISP

const string = o =>
  typeof o === 'string' ? o :
  Array.isArray(o) ? `(${o.map(string).join(' ')})` :
  JSON.stringify(o)
const log = o => { console.dir(o, {depth: null}); return o }
const fail = (m, ...a) => { throw new Error(m + ': ' + a.map(string).join(' ')) }
const parse = source => {
  if (source.trim().length === 0) {
    return []
  }
  // operator | symbols | string | number | id | white spaces
  const regexp = /(\.\.\.[A-Za-z_]*|[!+\-*/%<>:!=^|&]+|[()\[\]{}]|""".*?"""|"[^]*?(?<!\\)"|-?[0-9]+[0-9_]*(?:\.[0-9_]+)|[0-9A-Za-z_]+|(?:#[^\n]*|[ \n])+)/
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
      t.code === ':' ? parse_block() :
      t.code.startsWith('"""') ? {...t, code: '"' + t.code.slice(3, -3).replaceAll(/"/g, '\\"') + '"'} :
      t.code.match(/^[0-9]+[0-9.]*[xobe_]/) ? {...t, code: Number(t.code.replaceAll(/_/g, '')).toString()} :
      t)
  }
  const parse_exp = () => {
    const lhs = parse_unit()
    const is_op2 = s => typeof s === 'string' && s.match(/^:?[!+\-*/%<>!=^~|&]/) && s !== '!'
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

module.exports = { parse }

if (require.main === module) {
  const stringify = a => Array.isArray(a) ? `(${a.map(stringify).join(' ')})` : string(a.code)
  const assert = (expect, fact, src) => expect === fact ? process.stdout.write('.') : fail(`Expected: '${expect}'\n         Actual: '${fact}' source='${src}'`)
  const test = (expect, src) => assert(expect, stringify(parse(src)), src)

  // literal
  test('1', '1')
  test('(- 1)', '-1')
  test('1.0', '1.0')
  test('id', 'id')
  test('(f)', 'f()')
  test('"hi"', '"hi"')
  test('"h\\"i"', '"h\\"i"')
  test('"h\\"i"', "\"h\\\"i\"")
  test('" \\\\" "', '" \\\\" "')
  test('" \\" "', '""" " """')
  test('...', '...')
  test('...a', '...a')

  // syntax sugar for number
  test(  '1000',     '1e3')
  test(   '255',    '0xff')
  test(   '255',    '0xFF')
  test(     '9',    '0o11')
  test(     '7',   '0b111')
  test( '10000',  '10_000')
  test('0.1002', '0.1_002')

  // property access
  test('(. a b)', 'a.b')

  // single operator
  test('(! a)', '!a')
  test('((! a) b)', '!a b')
  test('(f (! a))', 'f !a')

  // binary operators
  test('(+ a b)', 'a + b')
  for (const op2 of '|| && + - * ** / % & | ^ << >> == != < <= > >= ='.split(' ')) {
    test(`(${op2} a b)`, `a ${op2} b`)
    test(`(a (${op2} b c) d)`, `a b ${op2} c d`)
  }

  // parentheses
  test('1', '(1)')
  test('(f 1)', '(f 1)')
  test('(+ 1 (+ 2 3))', '1 + 2 + 3')
  test('(+ (+ 1 2) 3)', '(1 + 2) + 3')

  // function call
  test('(f 1)', 'f 1')
  test('(f 1)', 'f(1)')
  test('(f (g 1))', 'f g(1)')
  test('(f g 1)', 'f g (1)')
  test('(f)', 'f()')
  test('(f (+ 1 2) 3)', 'f(1 + 2 3)')

  // method call
  test('(. f m)', 'f.m')
  test('(. f 1)', 'f.1')
  test('((. f m))', 'f.m()')
  test('((. f m) a)', 'f.m(a)')
  test('((. f m) a b)', 'f.m(a b)')
  test('((. ((. a f) 1) g) 2)', 'a.f(1).g(2)')

  // list
  test('(list)', '[]')
  test('(list 1)', '[1]')
  test('(list 1 2)', '[1 2]')

  // index access
  test('([ x 1)', 'x[1]')
  test('(x (list 1))', 'x [1]')
  test('([ x 1 2)', 'x[1 2]')
  test('(. ([ x a) b)', 'x[a].b')

  // indent
  test('(a b)', 'a:\n  b')
  test('(a (b c))', 'a:\n  b:\n    c')
  test('(a (b (__block c d)))', 'a:\n  b:\n    c\n    d')
  test('(a (__block (b c) d))', 'a:\n  b:\n    c\n  d')
  test('(__block (a (b c)) d)', 'a:\n  b:\n    c\nd')
  test('(__block (a (__block b (c d) e)) f)', 'a:\n  b\n  c:\n    d\n  e\nf')

  // block
  test('(__block a b)', 'a\nb')
  test('(__block (a b) c)', 'a b\nc')
  test('(__block a (b c))', 'a\nb c')
  test('(__block a b)', 'a;b')
  test('(__block (a b) (c d) (e f))', 'a b; c d; e f')
  test('(a (__block b c))', 'a: b; c')

  // priority of operators
  test('(&& (< a b) c)', 'a < b && c')
  test('(&& (== a b) c)', 'a == b && c')
  test('(&& (f) c)', 'f() && c')
  test('(+= a (* b c))', 'a += b * c')

  // comment
  test('(= a 1)', '#comment\na = 1 # comment\n#comment')
  test('(a (__block b c))', 'a:\n  #comment\n  b\n  #comment\n  c\n  # comment')

  // combinations
  test('(! (a b))', '!a(b)')
  test('(&& true (! false))', 'true && !false')
  test('(+ (a b) c)', 'a(b) + c')
  test('((. a b) c)', 'a.b c')
  test('(. ([ a b) c)', 'a[b].c')
  test('((. ([ a b) c) d)', 'a[b].c(d)')
  test('(. (list) a)', '[].a')
  test('(. (list) size)', '[].size')
  test('((. (list 1) m) a)', '[1].m a')
  test('((. (list 1) m) a)', '[1].m(a)')
//  test('((. (list 1) m) (=> x (>= x 1)))', '[1].m(x => x >= 1)')

//  // syntax sugar: arrow function
//  test('r"\\t"', 'r"\\t"')
//  test("r'\\t'", "r'\\t'")
//  test('(=> a b)', 'a => b')
//  test('(=> p (+ (. p x) (. p y)))', 'p => p.x + p.y')
//  test('(=> (a b) c)', 'a,b => c')
//  test('(=> (a b c) d)', 'a,b,c => d')
//  test('(=> a (b c))', 'a => b c')
//  test('(=> a (+ 1 2))', 'a => 1 + 2')
//  test('(=> a 1)', 'a =>\n  1')
//  test('(=> a (__block 1 2))', 'a =>\n  1\n  2')

  // edge case
  test('1', '1\n')
  test('()', '')
  test('()', '\n')
  test('(f a b)', 'f(a\nb\n)')

  console.log('ok')
}
