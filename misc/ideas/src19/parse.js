/*
 * Convert code to internal expression
 *
 * Syntax sugars
 * [x] f(...)      # (f ...)
 * [x]  .m         # (. m)
 * [x] o.m         # (. o m)
 * [x] a b         # (a b)
 * [x] a op2 b     # (op2 a b)
 * [x] op1 a       # (op1 a)
 * [x] a b: c      # (: (a b) (c))
 * [x] a b:
 *       c
 *       d e       # (: (a b) (c (d e)))
 * [x] {a b:c}     # (new a a b c)
 */
const str = o =>
  typeof o === 'string' ? o :
  o instanceof String ? o.toString() :
  Array.isArray(o) ? `(${o.map(str).join(' ')})` :
  JSON.stringify(o)
const put = (...a) => { process.stdout.write(a.map(str).join(' ')); return a[0] }
const puts = (...a) => { console.log(a.map(str).join(' ')); return a[0] }
const dump = o => { console.dir(o, {depth: null}); return o }
const fail = m => { throw new Error(m) }
const parse = source => {
  let pos = 0
  const tokens = source.split(/((?:!=)|[()\[\]{}!]|(?:[0-9]+(?:\.[0-9]+)?)|[ \t\r\n]+|[r$]?"[^"]*"|`[^`]*`|[A-Za-z0-9_]+)/).filter(t => t.length > 0)
  const binaryOps = '. , * ** / // % + - >> << ^ & | = += -= *= /= %= **= => < <= > >= == != === !== <=> && ||'.split(' ')
  const statement = () => {
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
    const consume = () => ((tokens[pos].match(/^[ \t]+$/) && ++pos), tokens[pos++])
    const until = end => many([], t => t === end ? ++pos : unit())
    const call = (o, a) => a.length === 0 ? ['__call', o] : [o].concat(a)
    const index = (o, a) => ['__get', o, ...a]
    const indent = s => s === undefined ? 0 : s.match(/[\r\n]/) ? s.split(/[\r\n]/).slice(-1)[0].length : -1
    const key = o => typeof o === 'string' ? JSON.stringify(o) : o
    const pairs = a => [...Array(a.length / 3).keys()].flatMap(i => [key(a[i*3]), a[(i*3)+2]])
    const container = a => a.length === 0 ? ['__call', 'list'] :
      a.length === 1 && a[0] === ':' ? ['__call', 'dict'] :
      a.length >= 3 && a[1] === ':' ? ['dict', ...pairs(a)] :
      ['list', ...a]
    const object = a => {
      if (a.length == 0) {
        return ['__call', 'new']
      }
      const pos = a.findIndex(s => Array.isArray(s) && s[0] === '=')
      return pos === -1 ? ['new', ...a] :
        ['new', ...a.slice(0, pos).flatMap(x => [x, x]), ...a.slice(pos).flatMap(x => [x[1], x[2]])]
    }
    const bottom = t =>
      tokens[pos] === '.' ? (++pos, ['.', t, consume()]) :
      tokens[pos] === '=' ? (++pos, ['=', t, unit()]) :
      t === '.' && tokens[pos - 2].match(/^[ \t]+$/) ? [t, consume()] :
      t === '[' ? container(until(']')) :
      t === '{' ? object(until('}')) :
      t === '(' ? until(')') :
      t
    const unit = () => _unit(bottom(consume()))
    const _unit = o => tokens[pos] === '(' && !' \t:'.includes(tokens[pos - 1]) ? ++pos && call(o, until(')')) :
                       tokens[pos] === '[' && !' \t:'.includes(tokens[pos - 1]) ? ++pos && index(o, until(']')) :
                       o
    const mark = (m, a) => a.length >= 2 ? [m, ...a] : a
    const block = a => a.slice(-1)[0] === ':' && tokens[pos].match(/[\r\n]/) ? [...a, statement()] : a
    const line = () => block(many([], t => !t.match(/[\r\n]/) && unit()))
    const lines = n => many([], t => indent(t) === n ? (++pos, line()) : false)
    return mark('__do', lines(indent(tokens[pos])))
  }
  const reorder = o => {
    const isOp2 = s => typeof s === 'string' && binaryOps.includes(s)
    const op2 = a => (!Array.isArray(a) || a.length <= 2) ? a :
      isOp2(a[1]) ? op2([prioritize(a[1], a[0], a[2]), ...a.slice(3)]) :
      [a[0], ...op2(a.slice(1))]
    const prioritize = (op, l, r) => Array.isArray(l) && isOp2(l[0]) && priority(op) < priority(l[0]) ? [l[0], l[1], [op, l[2], r]] : [op, l, r]
    const priority = op => binaryOps.findIndex(t => t == op)
    const isId = o => typeof o === 'string' && /^[a-zA-Z_]/.test(o)
    const block = a => _block(a, a.findIndex(t => t === ':'))
    const _block = (a, n) => n === -1 || !isId(a[0]) ? a : [a[n], a[0], a.slice(1, n), a.slice(n+1)]
    const declare = a => (pos => pos === -1 ? a : [a[pos], a.slice(0, pos), a.slice(pos+1)])(a.findIndex(t => t === '::'))
    const unnest = a => a.length === 1 ? a[0] : a
    const to_obj = o => typeof o === 'string' ? new String(o) : o
    return Array.isArray(o) ? (o.length === 1 ? reorder(o[0]) : unnest(block(declare(op2(o)))).map(reorder)) : to_obj(o)
  }
  const if_else = o => Array.isArray(o) ? (o[0] == '__do' ? _if_else(o) : o.map(if_else)) : o
  const _if_else = lines => {
    const ret = []
    for (const line of lines) {
      if (line[1] == 'else') {
        if (line[2][0] == 'if') {
          ret[ret.length - 1] = ret[ret.length - 1].concat(line[2].slice(1))
          ret[ret.length - 1] = ret[ret.length - 1].concat(line[3])
        } else {
          ret[ret.length - 1] = ret[ret.length - 1].concat(line[3])
        }
      } else {
        ret.push(line)
      }
    }
    return lines.length >= 3 && ret.length === 2 ? ret[1] : ret
  }
  tokens.unshift('\n')
  return if_else(reorder(statement()))
}

module.exports = { parse }

if (require.main === module) {
  const stringify = a => Array.isArray(a) ? `(${a.map(stringify).join(' ')})` : str(a)
  const assert = (expect, fact, src) => expect === fact ? put('.') : fail(`Expected '${expect}' but got '${fact}' in '${src}'`)
  const test = (expect, src) => assert(expect, stringify(parse(src)), src)
  test('(f (. int))', 'f .int')

  // primitives
  test('1', '1')
  test('1.0', '1.0')
  test('id', 'id')
  test('"hi"', '"hi"')
  test('r"hi"', 'r"hi"')
  test('$"hi"', '$"hi"')
  test('(__call list)', '[]')
  test('(list 1 2)', '[1 2]')
  test('(__call dict)', '[:]')
  test('(dict "a" 1)', '[a:1]')
  test('(dict "a" (+ 1 2))', '[a:(1+2)]')
  test('(dict (+ 1 2) (+ 3 4))', '[(1+2):(3+4)]')
  test('(dict "a" 1 "b" (+ 1 2) c (+ 3 4))', '[a:1 b:(1+2) (c):(3+4)]')
  test('(__call new)', '{}')
  test('(new a 1)', '{a=1}')
  test('(new a a b b c (+ 1 2) d 3)', '{a b c=(1+2) d=3}')
  test('(=> a a)', 'a => a')
  test('(=> (, a b) a)', 'a,b => a')
  test('(=> p (+ 1 2))', 'p => 1 + 2')
  test('(. int)', '.int')
  test('(f (. int))', 'f .int')

  // property access
  test('(. (__call list) length)', '[].length')
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
  test('(. f 1)', 'f.1')
  test('(__call (. f m))', 'f.m()')
  test('((. f m) a)', 'f.m(a)')
  test('((. f m) a b)', 'f.m(a b)')

  // index access
  test('(__get x 1)', 'x[1]')
  test('(__get x 1 2)', 'x[1 2]')

  // block
  test('(: a () b)', 'a: b')
  test('(: a b c)', 'a b: c')

  // if-else
  test('(: if (a b) c)', 'if a b: c')
  test('(: if (a b) c d)', 'if a b: c\nelse: d')

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

  // combinations
  test('(! (a b))', '!a(b)')
  test('(+ (a b) c)', 'a(b) + c')

  puts('ok')
}
