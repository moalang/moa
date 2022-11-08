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
  const tokens = source.split(/([()\[\]!]|[0-9.]+|[ \t\r\n]+)/).filter(t => t.length > 0)
  let pos = 0
  const inc = () => {
    const old = pos
    ++pos
    while (pos < tokens.length && /[ \t\r\n]/.test(tokens[pos])) {
      ++pos
    }
    return old
  }
  const many = (a, f, g) => {
    g ||= () => true;
    while(pos < tokens.length && g(tokens[pos])) {
      a.push(f(tokens[inc()]))
    };
    return a
  }
  const to_a = o => Array.isArray(o) ? o : [o]
  const until = (a, f, mark) => many(a, f, t => t !== mark || (inc() && false))
  const call = o => tokens[pos] === '(' ? _call(o, consume(tokens[pos])[0]) : o
  const _call = (o, a) => a.length === 0 ? ['__call', o] : to_a(o).concat(a)
  const bottom = t => tokens[pos] === '.' ? inc() && ['.', t, tokens[inc()]] :
    t === '[' ? until(['list'], consume, ']') :
    t === '(' ? until([], consume, ')') :
    t
  const consume = t => call(bottom(t))
  const unwrap = o => {
    const op2 = a => a.length <= 2 ? a :
      /[+\-*/%|&<>!=.]/.test(a[1]) && a[1] !== '=' ? [a[1], a[0], op2(a.slice(2))] :
      a.slice(0, 2).concat(op2(a.slice(2)))
    const def = a => _def(a, a.findIndex(t => t === '='))
    const _def = (a, n) => n === -1 ? a : ['=', a[0], a.slice(1, n), a.slice(n+1)]
    return Array.isArray(o) ? (o.length === 1 ? unwrap(o[0]) : def(op2(o)).map(unwrap)) : o
  }
  return unwrap(many([], consume))
}
const stringify = a => Array.isArray(a) ? `(${a.map(stringify).join(' ')})` : str(a)
const assert = (expect, fact, src) => expect === fact ? put('.') : fail(`Expected '${expect}' but got '${fact}' in '${src}'`)
const test = (expect, src) => assert(expect, stringify(convert(src)), src)
test('(__call (. f m))', 'f.m()')

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
test('(+ 1 (+ 2 3))', '1 + 2 + 3')

// parentheses
test('1', '(1)')
test('(f 1)', '(f 1)')
test('(+ 1 2)', '(1 + 2)')
test('(+ (+ 1 2) 3)', '(1 + 2) + 3')

// function call
test('(__call f)', 'f()')
test('(f 1)', 'f(1)')

// method call
test('(. f m)', 'f.m')
test('(__call (. f m))', 'f.m()')

// define function
test('(= f () 1)', 'f = 1')
test('(= f n n)', 'f n = n')
test('(= f (a b) (+ a b))', 'f a b = a + b')

puts('ok')
