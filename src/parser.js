/*
 * Convert code to internal expression
 *
 * Syntax sugars
 * [x] f(...)      # (f ...)
 * [x] f ...       # (f ...)
 * [x]  .m         # (. m)
 * [x] o.m         # (. o m)
 * [x] a b         # (a b)
 * [x] a op2 b     # (op2 a b)
 * [x] op1 a       # (op1 a)
 * [x] a b: c      # (: (a b) (c))
 * [x] a b:
 *       c
 *       d e       # (: (a b) (__pack c (d e)))
 * [x] f ... = ... # (= f (=> [...] ...))
 * [x] # comment
 */
const string = o =>
  typeof o === 'string' ? o :
  Array.isArray(o) ? `(${o.map(string).join(' ')})` :
  JSON.stringify(o)
const put = (...a) => { process.stdout.write(a.map(string).join(' ')); return a[0] }
const puts = (...a) => { console.log(a.map(string).join(' ')); return a[0] }
const log = o => { console.dir(o, {depth: null}); return o }
const fail = m => { throw new Error(m) }
const parse = source => {
  let pos = 0
  const trim = a => a.length === 0 ? [] :
    a[0].match(/^[ \r\n\t]/) ? trim(a.slice(1)) :
    a[a.length - 1].match(/^[ \r\n\t]/) ? trim(a.slice(0, -1)) : a
  const regexp = /((?:!=)|[()\[\]{}!]|(?:-?[0-9]+(?:\.[0-9]+)?)|[ \t\r\n]+(?:#[^\n]*|[ \t\r\n]+)*|r?`.*`|r?"[^]*?(?<!\\)"|[A-Za-z0-9_]+|#[^\n]*)/
  const tokens = trim(source.split(regexp).filter(t => t.length > 0 && !t.startsWith('#')))
  const not_space = t => !(typeof t === 'string' && t.match(/^[ \t\r\n#]/))
  const statement = () => {
    const many = (a, f) => pos >= tokens.length ? a : (ret =>
      typeof ret === 'string' || Array.isArray(ret) || ret === true ?
      many((a.push(ret), a), f) : a)(f(tokens[pos]))
    const consume = () => ((tokens[pos].match(/^[ \t]+$/) && ++pos), tokens[pos++])
    const until = end => many([], t => t === end ? ++pos : unit()).filter(not_space)
    const call = (o, a) => a.length === 0 ? ['__call', o] : [o].concat(a)
    const index = (o, a) => ['__index', o, ...a]
    const indent = s => s === undefined ? 0 : s.match(/[\r\n]/) ? s.split(/[\r\n]/).slice(-1)[0].length : -1
    const key = o => typeof o === 'string' ? JSON.stringify(o) : o
    const pairs = a => [...Array(a.length / 3).keys()].flatMap(i => [key(a[i*3]), a[(i*3)+2]])
    const unit = () => {
      const prefix = t =>
        t === '!' ? [t, unit()] :
        t === '[' ? call('list', until(']')) :
        t === '(' ? until(')') :
        t
      const suffix = o =>
        tokens[pos] === '(' ? ++pos && suffix(call(o, until(')'))) :
        tokens[pos] === '[' ? ++pos && suffix(index(o, until(']'))) :
        tokens[pos] === '.' ? (++pos, suffix(['.', o, consume()])) :
        tokens[pos] === '=' ? (++pos, ['=', o, unit()]) :
        o
      return suffix(prefix(consume()))
    }
    const block = a => [':', '=', '=>'].includes(a.slice(-1)[0]) && tokens[pos].match(/[\r\n]/) ? [...a, statement()] : a
    const line = () => block(many([], t => !t.match(/[\r\n]/) && unit()))
    const lines = n => many([], t => indent(t) === n ? (++pos, line()) : false)
    const pack = a => a.length >= 2 ? ['__pack', ...a] : a
    return pack(lines(indent(tokens[pos])))
  }
  const reorder = o => {
    const op2s = '. , * ** / // % + ++ - >> << ^ & | := += -= *= /= %= **= < <= > >= == != === !== <=> && || = =>'.split(' ')
    const is_op2 = s => typeof s === 'string' && op2s.includes(s)
    const priority = op => op2s.findIndex(t => t == op)
    const op2 = a => (!Array.isArray(a) || a.length <= 2) ? a :
      is_op2(a[1]) ? op2([prioritize(a[1], a[0], a[2]), ...a.slice(3)]) :
      [a[0], ...op2(a.slice(1))]
    const prioritize = (op, l, r) => Array.isArray(l) && is_op2(l[0]) && priority(op) < priority(l[0]) ? [l[0], l[1], [op, l[2], r]] : [op, l, r]
    const block = a => (n => n === -1 ? a : [a[n], a.slice(0, n), a.slice(n+1)])(a.findIndex(t => t === ':'))
    const declare = a => a[1] === '=' ? ['=', a[0], a.slice(2)] : a
    const unnest = a => a.length === 1 ? a[0] : a
    return Array.isArray(o) ? (o.length === 1 ? reorder(o[0]) : unnest(block(op2(declare(o)))).map(reorder)) : o
  }
  tokens.unshift('\n')
  return reorder(statement())
}

module.exports = { parse }

if (require.main === module) {
  const stringify = a => Array.isArray(a) ? `(${a.map(stringify).join(' ')})` : string(a)
  const assert = (expect, fact, src) => expect === fact ? put('.') : fail(`Expected '${expect}' but got '${fact}' source='${src}'`)
  const test = (expect, src) => assert(expect, stringify(parse(src)), src)
  //test('(+ 1 (* 2 3)', '1 + 2 * 3') // TODO fix me

  // primitives
  test('1', '1')
  test('1.0', '1.0')
  test('id', 'id')
  test('"hi"', '"hi"')
  test('"h\\"i"', '"h\\"i"')
  test('"\\\\""', '"\\\\""')
  test('r"\\t"', 'r"\\t"')
  test('(__call list)', '[]')
  test('(list 1 2)', '[1 2]')
  test('(=> a a)', 'a => a')
  test('(=> (, a b) a)', 'a,b => a')
  test('(=> p (+ 1 2))', 'p => 1 + 2')
  test('(=> p 1)', 'p =>\n  1')
  test('(=> p (__pack 1 2))', 'p =>\n  1\n  2')

  // property access
  test('(. a b)', 'a.b')
  test('((. a b) c)', 'a.b c')
  test('(. (__call list) size)', '[].size')
  test('(=> p (+ (. p x) (. p y)))', 'p => p.x + p.y')

  // single operator
  test('(! true)', '!true')

  // binary operators
  test('(+ 1 2)', '1 + 2')
  test('(+ (+ 1 2) 3)', '1 + 2 + 3')
  test('(!= 1 1)', '1 != 1')
  test('(= a 1)', 'a = 1')
  test('(+= a 1)', 'a += 1')
  test('(= a (+ 1 2))', 'a = 1 + 2')
  test('(+= a (+ 1 2))', 'a += 1 + 2')
  test('(+ 1 (* 2 3))', '1 + 2 * 3')
  test('(+ 1 (/ 2 3))', '1 + 2 / 3')
  test('(+ 1 (% 2 3))', '1 + 2 % 3')
  test('(+ 1 (// 2 3))', '1 + 2 // 3')
  test('(&& true (! false))', 'true && !false')

  // parentheses
  test('1', '(1)')
  test('(f 1)', '(f 1)')
  test('(+ 1 2)', '(1 + 2)')
  test('(+ (* 1 2) 3)', '1 * 2 + 3')
  test('(* 1 (+ 2 3))', '1 * (2 + 3)')
  //test('(+ 1 (* 2 3)', '1 + 2 * 3') // TODO fix me
  //test('(* (+ 1 2) 3)', '1 + 2 * 3') // TODO fix me

  // function call
  test('(__call f)', 'f()')
  test('(f 1)', 'f(1)')
  test('(f (+ 1 2))', 'f(1 + 2)')

  // method call
  test('(. f m)', 'f.m')
  test('(. f 1)', 'f.1')
  test('(__call (. f m))', 'f.m()')
  test('((. f m) a)', 'f.m(a)')
  test('((. f m) a b)', 'f.m(a b)')
  test('((. (list 1) m) a)', '[1].m a')
  test('((. (list 1) m) a)', '[1].m(a)')
  test('((. (list 1) m) (=> x (>= x 1)))', '[1].m(x => x >= 1)')
  test('((. ((. a f) 1) g) 2)', 'a.f(1).g(2)')

  // index access
  test('(__index x 1)', 'x[1]')
  test('(__index x 1 2)', 'x[1 2]')
  test('(. (__index x a) b)', 'x[a].b')

  // steps
  test('(__pack a b)', 'a\nb')
  test('(__pack (a b) c)', 'a b\nc')
  test('(__pack a (b c))', 'a\nb c')

  // assignment
  test('(= a b)', 'a = b')
  test('(= a (__pack b c))', 'a =\n  b\n  c')

  // block
  test('(: a b)', 'a: b')
  test('(: (a b) c)', 'a b: c')
  test('(: (a b) (c d))', 'a b: c d')

  // indent
  test('(: a b)', 'a:\n  b')
  test('(: a (: b c))', 'a:\n  b:\n    c')
  test('(: a (: b (__pack c d)))', 'a:\n  b:\n    c\n    d')
  test('(: a (__pack (: b c) d))', 'a:\n  b:\n    c\n  d')
  test('(__pack (: a (: b c)) d)', 'a:\n  b:\n    c\nd')
  test('(__pack (: a (__pack b (: c d) e)) f)', 'a:\n  b\n  c:\n    d\n  e\nf')

  // comment
  test('(= a 1)', '#comment\na = 1 # comment\n#comment')
  test('(: a (__pack b c))', 'a:\n  #comment\n  b\n  #comment\n  c\n  # comment')

  // combinations
  test('(! (a b))', '!a(b)')
  test('(+ (a b) c)', 'a(b) + c')
  test('(. (__index a b) c)', 'a[b].c')
  test('((. (__index a b) c) d)', 'a[b].c(d)')
  test('(. (__call list) a)', '[].a')

  // edge case
  test('1', '1\n')
  test('()', '')
  test('()', '\n')
  test('(= a (b c))', 'a = b c')
  test('(f (= a b))', 'f a = b')
  test('(f a b)', 'f(a\nb\n)')

  puts('ok')
}
