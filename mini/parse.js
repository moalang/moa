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
  const inc = () => {
    const old = pos
    ++pos
    many([], t => t, t => /[ \t\r\n]/.test(t))
    return old
  }
  const many = (a, f, g) => {
    g ||= () => true;
    while(pos < tokens.length && g(tokens[pos])) {
      a.push(f(tokens[inc()]))
    };
    return a
  }
  const until = (a, f, mark) => many(a, f, t => t !== mark || (inc() && false))
  const call = o => tokens[pos] === '(' && /[A-Za-z0-9_]/.test(tokens[pos - 1]) ? _call(o, consume(tokens[pos])[0]) : o
  const _call = (o, a) => a.length === 0 ? ['__call', o] : [o].concat(a)
  const bottom = t => tokens[pos] === '.' ? inc() && ['.', t, tokens[inc()]] :
    t === '[' ? until(['list'], consume, ']') :
    t === '(' ? until([], consume, ')') :
    t
  const consume = t => call(bottom(t))
  const unwrap = o => {
    const op2 = a => a.length <= 2 ? a :
      /[+\-*/%|&<>!=.]/.test(a[1]) && a[1] !== '=' ? op2([[a[1], a[0], a[2]], ...a.slice(3)]) :
      [a[0], ...op2(a.slice(1))]
    const def = a => _def(a, a.findIndex(t => t === '='))
    const _def = (a, n) => n === -1 ? a : ['=', a[0], a.slice(1, n), a.slice(n+1)]
    const block = a => _block(a, a.findIndex(t => t === ':'))
    const _block = (a, n) => n === -1 ? a : [':', a.slice(0, n), a.slice(n+1)]
    const unnest = a => a.length === 1 ? a[0] : a
    return Array.isArray(o) ? (o.length === 1 ? unwrap(o[0]) : unnest(block(def(op2(o)))).map(unwrap)) : o
  }
  return unwrap(many([], consume))
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

// indent based block
test('(: a b)', 'a: b')


puts('ok')
