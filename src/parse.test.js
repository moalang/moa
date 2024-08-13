const assert = require('node:assert/strict')
const { test } = require('node:test')

const { parse } = require('./parse.js')
const stringify = a => Array.isArray(a) ? `(${a.map(stringify).join(' ')})` : a.code
const assertParse = (expect, src) => assert.equal(stringify(parse(src)), expect, src)

test('parse literal', () => {
  assertParse('1', '1')
  assertParse('(- 1)', '-1')
  assertParse('1.0', '1.0')
  assertParse('id', 'id')
  assertParse('(f)', 'f()')
  assertParse('"hi"', '"hi"')
  assertParse('"h\\"i"', '"h\\"i"')
  assertParse('"h\\"i"', "\"h\\\"i\"")
  assertParse('" \\\\" "', '" \\\\" "')
  assertParse('" \\" "', '""" " """')
  assertParse('...', '...')
  assertParse('...a', '...a')
})

test('parse syntax sugar for number', () => {
  assertParse(  '1000',     '1e3')
  assertParse(   '255',    '0xff')
  assertParse(   '255',    '0xFF')
  assertParse(     '9',    '0o11')
  assertParse(     '7',   '0b111')
  assertParse( '10000',  '10_000')
  assertParse('0.1002', '0.1_002')
})

test('parse property access', () => {
  assertParse('(. a b)', 'a.b')
})

test('parse unary operator', () => {
  assertParse('(! a)', '!a')
  assertParse('((! a) b)', '!a b')
  assertParse('(f (! a))', 'f !a')
})

test('parse binary operator', () => {
  assertParse('(+ a b)', 'a + b')
  for (const op2 of '|| && + - * ** / % & | ^ << >> == != < <= > >= ='.split(' ')) {
    assertParse(`(${op2} a b)`, `a ${op2} b`)
    assertParse(`(a (${op2} b c) d)`, `a b ${op2} c d`)
  }
})

test('parse parentheses', () => {
  assertParse('1', '(1)')
  assertParse('(f 1)', '(f 1)')
  assertParse('(+ 1 (+ 2 3))', '1 + 2 + 3')
  assertParse('(+ (+ 1 2) 3)', '(1 + 2) + 3')
})

test('parse function call', () => {
  assertParse('(f 1)', 'f 1')
  assertParse('(f 1)', 'f(1)')
  assertParse('(f (g 1))', 'f g(1)')
  assertParse('(f g 1)', 'f g (1)')
  assertParse('(f)', 'f()')
  assertParse('(f (+ 1 2) 3)', 'f(1 + 2 3)')
})

test('parse method call', () => {
  assertParse('(. f m)', 'f.m')
  assertParse('(. f 1)', 'f.1')
  assertParse('((. f m))', 'f.m()')
  assertParse('((. f m) a)', 'f.m(a)')
  assertParse('((. f m) a b)', 'f.m(a b)')
  assertParse('((. ((. a f) 1) g) 2)', 'a.f(1).g(2)')
})

test('parse list literal', () => {
  assertParse('(list)', '[]')
  assertParse('(list 1)', '[1]')
  assertParse('(list 1 2)', '[1 2]')
})

test('parse index access', () => {
  assertParse('([ x 1)', 'x[1]')
  assertParse('(x (list 1))', 'x [1]')
  assertParse('([ x 1 2)', 'x[1 2]')
  assertParse('(. ([ x a) b)', 'x[a].b')
})

test('parse indent', () => {
  assertParse('(a (: b))', 'a:\n  b')
  assertParse('(a (: (b (: c))))', 'a:\n  b:\n    c')
  assertParse('(a (: (b (: (__block c d)))))', 'a:\n  b:\n    c\n    d')
  assertParse('(a (: (__block (b (: c)) d)))', 'a:\n  b:\n    c\n  d')
  assertParse('(__block (a (: (b (: c)))) d)', 'a:\n  b:\n    c\nd')
  assertParse('(__block (a (: (__block b (c (: d)) e))) f)', 'a:\n  b\n  c:\n    d\n  e\nf')
})

test('parse block', () => {
  assertParse('(__block a b)', 'a\nb')
  assertParse('(__block (a b) c)', 'a b\nc')
  assertParse('(__block a (b c))', 'a\nb c')
  assertParse('(__block a b)', 'a;b')
  assertParse('(__block (a b) (c d) (e f))', 'a b; c d; e f')
  assertParse('(a (: (__block b c)))', 'a: b; c')
})

test('parse priority of operators', () => {
  assertParse('(&& (< a b) c)', 'a < b && c')
  assertParse('(&& (== a b) c)', 'a == b && c')
  assertParse('(&& (f) c)', 'f() && c')
  assertParse('(+= a (* b c))', 'a += b * c')
})

test('parse comment', () => {
  assertParse('(= a 1)', '#comment\na = 1 # comment\n#comment')
  assertParse('(a (: (__block b c)))', 'a:\n  #comment\n  b\n  #comment\n  c\n  # comment')
})

test('parse combinations', () => {
  assertParse('(! (a b))', '!a(b)')
  assertParse('(&& true (! false))', 'true && !false')
  assertParse('(+ (a b) c)', 'a(b) + c')
  assertParse('((. a b) c)', 'a.b c')
  assertParse('(. ([ a b) c)', 'a[b].c')
  assertParse('((. ([ a b) c) d)', 'a[b].c(d)')
  assertParse('(. (list) a)', '[].a')
  assertParse('(. (list) size)', '[].size')
  assertParse('((. (list 1) m) a)', '[1].m a')
  assertParse('((. (list 1) m) a)', '[1].m(a)')
  //  assertParse('((. (list 1) m) (=> x (>= x 1)))', '[1].m(x => x >= 1)')

  //  // syntax sugar: arrow function
  //  assertParse('r"\\t"', 'r"\\t"')
  //  assertParse("r'\\t'", "r'\\t'")
  //  assertParse('(=> a b)', 'a => b')
  //  assertParse('(=> p (+ (. p x) (. p y)))', 'p => p.x + p.y')
  //  assertParse('(=> (a b) c)', 'a,b => c')
  //  assertParse('(=> (a b c) d)', 'a,b,c => d')
  //  assertParse('(=> a (b c))', 'a => b c')
  //  assertParse('(=> a (+ 1 2))', 'a => 1 + 2')
  //  assertParse('(=> a 1)', 'a =>\n  1')
  //  assertParse('(=> a (__block 1 2))', 'a =>\n  1\n  2')
})

test('parse edge case', () => {
  assertParse('1', '1\n')
  assertParse('()', '')
  assertParse('()', '\n')
  assertParse('(f a b)', 'f(a\nb\n)')
})
