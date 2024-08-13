const assert = require('node:assert')
const { test } = require('node:test')

const { parse } = require('./parse.js')
const { infer, TypeError } = require('./infer.js')

const showType = type => {
  const show = t => t.instance ? show(t.instance) :
    t.dot3 ? '...' :
    t.name && t.types ? t.name + '[' + t.types.map(show).join(' ') + ']' :
    t.types ? '(' + t.types.map(show).join(' ') + ')' :
    t.props ? '(' + t.props.map(([f,t]) => `${f}.${show(t)}`).join(' ') + ')' :
    t.name + (t.label ? '.' + t.label : '')
  const s = show(type)
  const o = {}
  const r = s.replaceAll(/\b\d+/g, t => o[t] ||= Object.keys(o).length + 1)
  return r
}
const assertInferThrows = src => assert.throws(() => infer(parse(src)), TypeError)
const assertInfer = (expect, src) => {
  const node = parse(src)
  try {
    infer(node)
    const actual = showType(node.type)
    assert.deepStrictEqual(actual, expect)
  } catch (e) {
    console.log('Failed')
    console.log('   src:', src)
    console.dir(e, {depth: null})
    process.exit(1)
  }
}

test('infer litral', () => {
  assertInfer('bool', 'true')
  assertInfer('bool', 'false')
  assertInfer('int', '1')
  assertInfer('float', '1.0')
})

test('infer constructor', () => {
  'int i8 i16 i32 i64 u8 u16 u32 u64 float f32 f64'.split(' ').map(t => assertInfer(t, `${t} 0`))
  assertInfer('list[int]', 'list 1')
  assertInfer('set[int]', 'set 1')
  assertInfer('dict[int bool]', 'dict 1 true')
})

test('infer exp', () => {
  assertInfer('int', '- 1')
  assertInfer('int', '+ 1 1')
  assertInfer('float', '+ 1.0 1.0')
  assertInfer('bool', '< 1 1')
})

test('infer branch', () => {
  assertInfer('int', 'iif true 1 2')
  assertInfer('bool', 'iif true true true')
})

test('infer var and let', () => {
  assertInfer('int', 'let a 1')
  assertInfer('int', 'var a 1')
  assertInfer('int', 'var a 1\na')
  assertInfer('int', 'var a 1: a+=1\na')
})

test('infer function', () => {
  assertInfer('(int)', 'def _: 1')
  assertInfer('(int int)', 'def _ a: + a 1')
  assertInfer('(1.num 1.num 1.num)', 'def _ a b: + a b')
})

test('infer generics function', () => {
  assertInfer('(1 1)', 'def _ a: a')
  assertInfer('(1 2 1)', 'def _ a b: a')
  assertInfer('(1 2 2)', 'def _ a b: b')
  assertInfer('int', 'def f a: a\nf 1')
  assertInfer('bool', 'def f a: a\nf 1\nf true')
})

test('infer complex function', () => {
  assertInfer('int',                           'def f x: + x 1\ndef g x: + x 2\n+ (f 1) (g 1)')
  assertInfer('((1 2) (2 3) 1 3)',             'def _ f g x: g (f x)')
  assertInfer('((1 2 3) (1 2) 1 3)',           'def _ x y z: x z (y z)')
  assertInfer('(1 (1 bool) (1 1))',            'def _ b x: iif (x b) x (def _ x: b)')
  assertInfer('(bool bool)',                   'def _ x: iif true x (iif x true false)')
  assertInfer('(bool bool bool)',              'def _ x y: iif x x y')
  assertInfer('(1 1)',                         'def _ n: (def _ x: x (def _ y y)) (def _ f: f n)')
  assertInfer('((1 2) 1 2)',                   'def _ x y: x y')
  assertInfer('((1 2) ((1 2) 1) 2)',           'def _ x y: x (y x)')
  assertInfer('(1 ((1 2 3) 4 2) (1 2 3) 4 3)', 'def _ h t f x: f h (t f x)')
  assertInfer('((1 1 2) ((1 1 2) 1) 2)',       'def _ x y: x (y x) (y x)')
  assertInfer('(((1 1) 2) 2)',                 'def id x: x\ndef f y: id (y id)')
  assertInfer('(int)',                         'def id x x\ndef f: iif (id true) (id 1) (id 2)')
  assertInfer('(int)',                         'def f x (3)\ndef g: + (f true) (f 4)')
  assertInfer('(bool (1 1))',                  'def f x x\ndef g y y\ndef h b (iif b (f g) (g f))')
})

test('infer declare function', () => {
  assertInfer('(bool)', 'dec _: bool')
  assertInfer('(int)', 'dec _: int')
  assertInfer('(int bool)', 'dec _: int bool')
  assertInfer('(... int)', 'dec _: ... int')
  assertInfer('(1 1)', 'dec _ a: a a')
  assertInfer('(1 1 2)', 'dec _ a b: a a b')
})

test('infer variadic arguments', () => {
  assertInfer('(... int)', 'def f ...: 1')
  assertInfer('int', 'def f ...: 1\nf 1')
  assertInfer('int', 'def f ...: 1\nf 1 true')
  assertInfer('(... 1)', 'def f ...a: a')
  assertInfer('tuple[int]', 'def f ...a: a\nf 1')
  assertInfer('tuple[int bool]', 'def f ...a: a\nf 1 bool')
  assertInfer('bool', 'def f[t] ...[t]: true\nf 1 2')
  assertInfer('int', 'def f[t] ...[t]: t\nf 1 2')
  assertInfer('tuple[int bool int bool]', 'def f[t u] ...a[t u]: a\nf 1 true 2 false')
})

test('infer class', () => {
  assertInfer('(x.int)', 'class _: x int')
  assertInfer('int', 'class c: x int\n. c(1) x')
  assertInfer('(x.1)', 'class _ a: x a')
  assertInfer('int', 'class c a: x a\n. c(1) x')
  assertInfer('bool', 'class c a: x a\n. c(true) x')
  assertInfer('(x.1)', 'class _ a: x a')
  assertInfer('(f.(int))', 'class _: f : int')
  assertInfer('(f.(int int))', 'class _: f : int int')
  assertInfer('(f.(1 2 int))', 'class _ a: f b: a b int')
  assertInfer('(f.(1 2 3 4))', 'class _ a b: f c d: a b c d')
})

test('infer method call', () => {
  assertInfer('(int)', '1.abs')
  assertInfer('int', '1.abs()')
  assertInfer('int', '1.neg()')
  assertInfer('float', '1.0.abs()')
  assertInfer('float', '1.0.neg()')
})

test('infer type error', () => {
  assertInferThrows('(+ true true)')
  assertInferThrows('(+ 1 true)')
  assertInferThrows('(+ 1 1.0)')
  assertInferThrows('def f[t] ...[t]: true\nf 1 true')
  assertInferThrows('def f[t u] ...[t u]: true\nf 1 true true')
})
