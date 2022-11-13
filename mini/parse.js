/*
 * This program convert Moa program to internal expression to runtime.
 * Moa program is optimized for developers to read and write.
 * The internal expression is optimized for machine, which is inspired by Lisp style.
 *
 * Syntax sugars
 * - f(...)      # (f ...)
 * - o.m         # (. o m)
 * - a b         # (a b)
 * - a = b       # (= a () b)
 * - a b = c     # (c = a b c)
 * - a b c = d   # (c = a (b c) d)
 * - a op2 b     # (op2 a b)
 * - op1 a       # (op1 a)
 * - a b: c      # (: (a b) (c))
 * - a b:
 *   c
 *   d e         # (: (a b) (c (d e)))
*/
const str = o =>
  typeof o === 'string' ? o :
  Array.isArray(o) ? `(${o.map(str).join(' ')})` :
  JSON.stringify(o)
const put = (...a) => { process.stdout.write(a.map(str).join(' ')); return a[0] }
const puts = (...a) => { console.log(a.map(str).join(' ')); return a[0] }
const dump = o => { console.dir(o, {depth: null}); return o }
const fail = m => { throw new Error(m) }
const convert = source => {
  const tokens = source.split(/([()\[\]!]|[0-9.]+|[ \t\r\n]+|"[^"]*"|`[^`]*`|[A-Za-z0-9_]+)/).filter(t => t.length > 0)
  let pos = 0
  const many = (a, f) => {
    while (pos < tokens.length) {
      const ret = f(tokens[pos])
      if (typeof ret === 'string' || Array.isArray(ret) || ret === true) {
        a.push(ret)
      } else {
        break
      }
    }
    return a
  }
  const chomp = t => (pos < tokens.length && tokens[pos].match(/^[ \t]+/) && ++pos, t)
  const consume = () => chomp(tokens[pos++])
  const until = (a, end) => many(a, t => t === end ? ++pos : unit())
  const call = o => tokens[pos] === '(' && !' \t'.includes(tokens[pos - 1]) ? ++pos && _call(o, until([], ')')) : o
  const _call = (o, a) => a.length === 0 ? ['__call', o] : [o].concat(a)
  const indent = s => s === undefined ? 0 : s.match(/[\r\n]/) ? s.split(/[\r\n]/).slice(-1)[0].length : -1
  const bottom = t =>
    tokens[pos] === '.' ? (consume(), ['.', t, consume()]) :
    t === '[' ? until(['list'], ']') :
    t === '(' ? until([], ')') :
    t
  const unit = () => call(bottom(consume()))
  const unwrap = o => {
    const op2 = a => a.length <= 2 ? a :
      '+-*/%|&<>!=.'.includes(a[1]) && a[1] !== '=' && a[1] !== ':' ? op2([[a[1], a[0], a[2]], ...a.slice(3)]) :
      [a[0], ...op2(a.slice(1))]
    const block = a => _block(a, a.findIndex(t => ':='.includes(t)))
    const _block = (a, n) => n === -1 ? a : [a[n], a[0], a.slice(1, n), a.slice(n+1)]
    const unnest = a => a.length === 1 ? a[0] : a
    return Array.isArray(o) ? (o.length === 1 ? unwrap(o[0]) : unnest(block(op2(o))).map(unwrap)) : o
  }
  const mark = (m, a) => a.length >= 2 ? [m, ...a] : a
  const block = a => ':='.includes(a.slice(-1)[0]) && tokens[pos].match(/[\r\n]/) ? [...a, lines(indent(tokens[pos]))] : a
  const line = () => block(many([], t => !t.match(/[\r\n]/) && unit()))
  const lines = n => mark('__do', many([], t => indent(t) === n ? (++pos, line()) : false))
  tokens.unshift('\n')
  return unwrap(lines(0))
}
const stringify = a => Array.isArray(a) ? `(${a.map(stringify).join(' ')})` : str(a)
const assert = (expect, fact, src) => expect === fact ? put('.') : fail(`Expected '${expect}' but got '${fact}' in '${src}'`)
const test = (expect, src) => assert(expect, stringify(convert(src)), src)

// primitives
test('1', '1')
test('1.0', '1.0')
test('id', 'id')
test('"hi"', '"hi"')
test('list', '[]')
test('(list 1)', '[1]')

// single operator
test('(! true)', '!true')

// binary operators
test('(+ 1 2)', '1 + 2')
test('(+ (+ 1 2) 3)', '1 + 2 + 3')

// parentheses
test('1', '(1)')
test('(f 1)', '(f 1)')
test('(+ 1 2)', '(1 + 2)')
test('(+ 1 (+ 2 3))', '1 + (2 + 3)')

// function call
test('(__call f)', 'f()')
test('(f 1)', 'f(1)')
test('(f (+ 1 2))', 'f(1 + 2)')

// method call
test('(. f m)', 'f.m')
test('(__call (. f m))', 'f.m()')
test('((. f m) a)', 'f.m(a)')
test('((. f m) a b)', 'f.m(a b)')

// define function
test('(= f () 1)', 'f = 1')
test('(= f n n)', 'f n = n')
test('(= f (a b) (+ a b))', 'f a b = a + b')

// block
test('(: a () b)', 'a: b')
test('(: a b c)', 'a b: c')

// top level
test('(__do a b)', 'a\nb')
test('(__do (a b) c)', 'a b\nc')
test('(__do a (b c))', 'a\nb c')

// indent
test('(: a () b)', 'a:\n  b')
test('(: a () (: b () c))', 'a:\n  b:\n    c')
test('(: a () (: b () (__do c d)))', 'a:\n  b:\n    c\n    d')
test('(: a () (__do (: b () c) d))', 'a:\n  b:\n    c\n  d')
test('(__do (: a () (: b () c)) d)', 'a:\n  b:\n    c\nd')
test('(__do (: a () (__do b (: c () d) e)) f)', 'a:\n  b\n  c:\n    d\n  e\nf')

puts('ok')
