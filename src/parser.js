// Convert Moa code to internal expression which looks like LISP

const string = o =>
  typeof o === 'string' ? o :
  Array.isArray(o) ? `(${o.map(string).join(' ')})` :
  JSON.stringify(o)
const put = (...a) => { process.stdout.write(a.map(string).join(' ')); return a[0] }
const puts = (...a) => { console.log(a.map(string).join(' ')); return a[0] }
const log = o => { console.dir(o, {depth: null}); return o }
const fail = (m, ...a) => { throw new Error(m + ': ' + a.map(string).join(' ')) }
const parse = source => {
  if (source.trim().length === 0) {
    return []
  }
  const regexp = /([!+\-*/%<>:!=^|&]+|[()\[\]{}]|r?"[^]*?(?<!\\)"|r?'[^]*?(?<!\\)'|-?[0-9]+(?:\.[0-9]+)|[0-9A-Za-z_]+|(?:#[^\n]*|[ \n])+)/ // operator | parenthesis | string | number | id | comment and spaces
  const tokens = source.trim().split(regexp).filter(s => s.length)

  var pos = 0
  const read = () => (t => t.match(/^[ #]/) ? tokens[++pos] || '' : t)(tokens[pos] || '')
  const loop = (a, f) => pos < tokens.length ? (v => v ? loop(a.concat([v]), f) : a)(f(read())) :a
  const many = f => loop([], f)
  const until = s => many(t => (t.includes('\n') && ++pos, read() === s ? (++pos, null) : parse_exp()))
  const consume = () => (t => ++pos && t)(read() || fail('out of index', pos, tokens))
  const indent = t => t.includes('\n') ? t.split('\n').at(-1).length : fail('not break line', t)
  const call = a => a.length === 1 ? ['__call', ...a] : a
  const squash = a => a.length === 1 ? a[0] : a
  const pack = a => a.length === 1 ? a[0] : a.length > 1 ? ['__pack', ...a] : a
  const parse_unit = () => {
    const suffix = t => {
      const close = tokens[pos] || ''
      const next = read()
      return close === '('  ? ++pos && suffix(call([t, ...until(')')])) :
             close === '['  ? ++pos && suffix(call(['__index', t, ...until(']')])) :
             next  === ','  ? suffix([t, ...many(t => t === ',' && ++pos && consume())]) :
             next  === '.'  ? ++pos && suffix([next, t, consume()]) :
             next  === '=>' ? ++pos && [next, t, parse_block()] :
             t
    }
    const t = consume()
    return suffix(
      t === '!' ? [t, parse_unit()] :
      t === '-' ? [t, parse_unit()] :
      t === '[' ? call(['list', ...until(']')]) :
      t === '(' ? squash(until(')')) :
      t === ':' ? parse_block() :
      t)
  }
  const parse_exp = () => {
    const lhs = parse_unit()
    const is_op2 = s => typeof s === 'string' && s.match(/^:?[!+\-*/%<>!=^~|&]/) && s !== '!'
    const op2s = '< &&'.split(' ')
    const priority = op => op2s.findIndex(x => x === op)
    const sort = (op, lhs, rhs) =>
      Array.isArray(rhs) && is_op2(rhs[0]) && priority(op) < priority(rhs[0]) ? [rhs[0], [op, lhs, rhs[1]], rhs[2]] :
      [op, lhs, rhs]
    return is_op2(read()) ? (op => sort(op, lhs, parse_exp()))(consume()) : lhs
  }
  const is_stop = t => t.includes('\n') || t === ')' || t === ']' || t === ';'
  const parse_line = () => {
    const a = squash(many(t => !is_stop(t) && parse_exp()))
    const remain = []
    while (read() === ";" && ++pos) {
      remain.push(squash(many(t => !is_stop(t) && parse_exp())))
    }
    return remain.length ? pack([a, ...remain]) : a.length && a
  }
  const parse_lines = n => pack(many(t => (t.includes('\n') && indent(t) === n && ++pos, parse_line())))
  const parse_block = () => (t => t.includes('\n') ? parse_lines(indent(t)) : parse_line())(read())
  return parse_lines(0)
}

module.exports = { parse }

if (require.main === module) {
  const stringify = a => Array.isArray(a) ? `(${a.map(stringify).join(' ')})` : string(a)
  const assert = (expect, fact, src) => expect === fact ? put('.') : fail(`Expected: '${expect}'\n         Actual: '${fact}' source='${src}'`)
  const test = (expect, src) => assert(expect, stringify(parse(src)), src)

  // primitives
  test('1', '1')
  test('(- 1)', '-1')
  test('1.0', '1.0')
  test('id', 'id')
  test('"hi"', '"hi"')
  test('"h\\"i"', '"h\\"i"')
  test('"h\\"i"', "\"h\\\"i\"")
  test('"\\\\""', '"\\\\""')
  test('r"\\t"', 'r"\\t"')
  test("r'\\t'", "r'\\t'")
  test('(=> a b)', 'a => b')

  // container
  test('(__call list)', '[]')
  test('(list 1)', '[1]')
  test('(list 1 2)', '[1 2]')

  // property access
  test('(. a b)', 'a.b')

  // single operator
  test('(! a)', '!a')
  test('((! a) b)', '!a b')
  test('(f (! a))', 'f !a')

  // binary operators
  test('(+ a b)', 'a + b')
  test('((+ a b) c)', 'a + b c')

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
  test('(__call f)', 'f()')
  test('(f (+ 1 2) 3)', 'f(1 + 2 3)')

  // method call
  test('(. f m)', 'f.m')
  test('(. f 1)', 'f.1')
  test('(__call (. f m))', 'f.m()')
  test('((. f m) a)', 'f.m(a)')
  test('((. f m) a b)', 'f.m(a b)')
  test('((. ((. a f) 1) g) 2)', 'a.f(1).g(2)')

  // index access
  test('(__index x 1)', 'x[1]')
  test('(x (list 1))', 'x [1]')
  test('(__index x 1 2)', 'x[1 2]')
  test('(. (__index x a) b)', 'x[a].b')

  // indent
  test('(a b)', 'a:\n  b')
  test('(a (b c))', 'a:\n  b:\n    c')
  test('(a (b (__pack c d)))', 'a:\n  b:\n    c\n    d')
  test('(a (__pack (b c) d))', 'a:\n  b:\n    c\n  d')
  test('(__pack (a (b c)) d)', 'a:\n  b:\n    c\nd')
  test('(__pack (a (__pack b (c d) e)) f)', 'a:\n  b\n  c:\n    d\n  e\nf')

  // statement
  test('(__pack a b)', 'a\nb')
  test('(__pack (a b) c)', 'a b\nc')
  test('(__pack a (b c))', 'a\nb c')
  test('(__pack a b)', 'a;b')
  test('(__pack (a b) (c d) (e f))', 'a b; c d; e f')
  test('(a (__pack b c))', 'a: b; c')

  // priority of operators
  test('(&& (< a b) c)', 'a < b && c')
  test('(&& (__call f) c)', 'f() && c')

  // comment
  test('(= a 1)', '#comment\na = 1 # comment\n#comment')
  test('(a (__pack b c))', 'a:\n  #comment\n  b\n  #comment\n  c\n  # comment')

  // combinations
  test('(! (a b))', '!a(b)')
  test('(&& true (! false))', 'true && !false')
  test('(+ (a b) c)', 'a(b) + c')
  test('(. (__index a b) c)', 'a[b].c')
  test('((. (__index a b) c) d)', 'a[b].c(d)')
  test('(. (__call list) a)', '[].a')
  test('((. a b) c)', 'a.b c')
  test('(. (__call list) size)', '[].size')
  test('((. (list 1) m) a)', '[1].m a')
  test('((. (list 1) m) a)', '[1].m(a)')
  test('((. (list 1) m) (=> x (>= x 1)))', '[1].m(x => x >= 1)')
  test('(=> p (+ (. p x) (. p y)))', 'p => p.x + p.y')
  test('(=> (a b) c)', 'a,b => c')
  test('(=> (a b c) d)', 'a,b,c => d')
  test('(=> a (b c))', 'a => b c')
  test('(=> a (+ 1 2))', 'a => 1 + 2')
  test('(=> a 1)', 'a =>\n  1')
  test('(=> a (__pack 1 2))', 'a =>\n  1\n  2')

  // edge case
  test('1', '1\n')
  test('()', '')
  test('()', '\n')
  test('(f a b)', 'f(a\nb\n)')

  puts('ok')
}
