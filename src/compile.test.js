const assert = require('node:assert')
const { test } = require('node:test')

const { parse } = require('./parse.js')
const { compile } = require('./compile.js')

const hint = x => Array.isArray(x) ? rec([], x) : x
const rec = (acc, a) => a.length === 0 ? acc :
  a[1]?.code === '@' ? rec(acc.concat((a[0].type = a[2].code, a[0])), a.slice(3)) :
  rec(acc.concat([ Array.isArray(a[0]) ? hint(a[0]) : a[0] ]), a.slice(1))
const assertCompile = (expect, src) => {
  try {
    const actual = compile(hint(parse(src)))
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
