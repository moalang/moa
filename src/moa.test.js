'use strict'
const assert = require('node:assert/strict')
const { test } = require('node:test')
const { readFileSync } = require('node:fs')
const path = require('node:path')

const { parse, infer, compileToJs, runtimeJs, TypeError } = require('./moa.js')

function assertCompile (expect, src) {
  const hint = x => Array.isArray(x) ? rec([], x) : x
  const rec = (acc, a) => a.length === 0 ? acc :
    a[1]?.code === '@' ? rec(acc.concat((a[0].type = a[2].code, a[0])), a.slice(3)) :
    rec(acc.concat([ Array.isArray(a[0]) ? hint(a[0]) : a[0] ]), a.slice(1))
  try {
    const actual = compileToJs(hint(parse(src)))
    assert.equal(actual, expect)
  } catch (e) {
    console.log('Failed')
    console.log('   src:', src)
    console.dir(e, {depth: null})
    process.exit(1)
  }
}

test('compile literal', () => {
  assertCompile('true', 'true')
  assertCompile('false', 'false')
  assertCompile('1', '1')
  assertCompile('1.1', '1.1')
  assertCompile('"a"', '"a"')
  assertCompile('id', 'id')
})

test('compile embedded', () => {
  for (const name of 'bool int float string i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 tuple list set dict log assert'.split(' ')) {
    assertCompile(`___${name}`, `${name}`)
    assertCompile(`___${name}()`, `${name}()`)
    assertCompile(`___${name}(1)`, `${name} 1`)
    assertCompile(`___${name}(1, a)`, `${name} 1 a`)
  }
})

test('compile operator', () => {
  assertCompile('(~1)', '~1')
  assertCompile('(!true)', '!true')
  for (const op of '|| && + - * ** / % & | ^ << >> == != < <= > >='.split(' ')) {
    assertCompile(`(1 ${op} 2)`, `1${op}2`)
    assertCompile(`(1 ${op} 2)`, `1 ${op} 2`)
  }
})

test('compile function', () => {
  assertCompile('f(1)', 'f 1')
  assertCompile('___int(1)', 'int 1')
})

//test('compile global', () => {
//  assertCompile('___float_inf', '. float inf')
//  assertCompile('___float_nan', '. float nan')
//})

test('compile property access', () => {
  assertCompile('___string_size(s)', '. s@string size')
})

test('compile method call', () => {
  assertCompile('___int_char(97)()', '(. 97@int char)()')
})

test('compile iif', () => {
  assertCompile('(1 ? 2 : 3)', 'iif 1 2 3')
  assertCompile('(1 ? 2 : 3 ? 4 : 5)', 'iif 1 2 3 4 5')
})

test('compile if / else', () => {
  assertCompile('if (a) {b}', 'if a: b')
  assertCompile('if (a) {b}\nelse {c}', 'if a: b\nelse: c')
  assertCompile('if (a) {b}\nelse if (c) {d}\nelse {e}', 'if a: b\nelse if c: d\nelse: e')
})

test('compile switch', () => {
  assertCompile('(__s => __s.__tag === "t.b" ? c : moa.throw("switch", __s))(a)', 'switch a@t: b: c')
  assertCompile('(__s => __s.__tag === "t.b" ? (c => c)(__s.__val) : moa.throw("switch", __s))(a)', 'switch a@t: b c: c')
})

test('compile throw / catch', () => {
  assertCompile('___throw(a)', 'throw a')
  assertCompile('(() => { try { return f(1) } catch (e) { return g(___error(e)) } })()', 'catch (f 1) g')
})

test('compile return', () => {
  assertCompile('return(a)', 'return a')
})

test('compile for loop', () => {
  assertCompile('for (let i=0; i<4; ++i) {a}',      'for i 4: a')
  assertCompile('for (let i=0; i<4; ++i) {a\nb}', 'for i 4: a; b')
  assertCompile('for (let i=1; i<4; ++i) {a}',      'for i 1 4: a')
  assertCompile('for (let i=1; i<4; i+=2) {a}',     'for i 1 4 2: a')
})

test('compile while loop', () => {
  assertCompile('while (a) {b}',      'while a: b')
  assertCompile('while (a) {b\nc}', 'while a: b; c')
})

test('compile continue and break', () => {
  assertCompile('continue', 'continue')
  assertCompile('break', 'break')
})

test('compile let and var', () => {
  assertCompile('const a = 1', 'let a 1')
  assertCompile('let a = 1', 'var a 1')
})

test('compile def', () => {
  assertCompile('function f() {return 1}', 'def f: 1')
  assertCompile('function f() {1\nreturn 2}', 'def f: 1; 2')
})

test('compile class', () => {
  assertCompile('function a(b) { return {b} }', 'class a: b int')
  assertCompile('function a(b, c) { return {b, c} }', 'class a: b int; c string')
  assertCompile('function a(b, c) { return {b, c} }', 'class a t: b int; c string')
})

test('compile enum', () => {
  assertCompile('const b = {__tag: "a.b"}', 'enum a: b')
  assertCompile('const b = {__tag: "a.b"}\nconst c = {__tag: "a.c"}', 'enum a: b; c')
  assertCompile('function b(__val) { return {__tag: "a.b", __val} }', 'enum a: b int')
  assertCompile('function b(c) { return {__tag: "a.b", __val: {c}} }', 'enum a: b: c int')
})

test('compile dec, interface and extern', () => {
  assertCompile('', 'dec f a')
  assertCompile('', 'interface a: b')
  assertCompile('', 'extern a: b')
})

function assertInferThrows(src) {
  assert.throws(() => infer(parse(src)), TypeError)
}
function assertInfer(expect, src) {
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

function assertParse(expect, src) {
  const stringify = a => Array.isArray(a) ? `(${a.map(stringify).join(' ')})` : a.code
  assert.equal(stringify(parse(src)), expect, src)
}

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

test('core test', () => {
  const code = readFileSync(path.join(__dirname, '..', 'test', 'core.moa'), 'utf-8')
  const tests = code.replaceAll(/#.*/g, '').split(/(?=assert )/).map(s => s.trim()).filter(s => s)
  for (const test of tests.slice(0, 34)) {
    try {
      const ast = parse(test)
      infer(ast)
      const js = compileToJs(ast)
      assert.equal(true, eval(runtimeJs + js), {ast, js})
    } catch (e) {
      console.log('echo', JSON.stringify(test), ' | node src/moa.js to js')
      throw e
    }
  }
})
