'use strict'
const assert = require('node:assert/strict')
const test = require('node:test').test
const vm = require('node:vm')

const { main, toJs, runtimeJs } = require('./moa.js')

function testToJs(expect, src) {
  const js = toJs(src)
  const actual = vm.runInNewContext(runtimeJs + js)
  assert.deepEqual(actual, expect, `${src} -> ${js} -> ${actual}`)
}

test('command line', () => {
  assert.match(main().out, /Usage:/)
  assert.deepEqual(true, vm.runInNewContext(main('to', ['js', 'true']).out))
})

test('literal', () => {
  testToJs(true, 'true')
  testToJs(false, 'false')
  testToJs(1, '1')
  testToJs(1.1, '1.1')
  testToJs(1000, '1e3')
  testToJs(1200, '1.2e3')
  testToJs(255, '0xff')
  testToJs(1000, '1_000')
  testToJs('a', '"a"')
  testToJs('a"b', '"""a"b"""')
})

test('op1', () => {
  testToJs(false, '!true')
  testToJs(-1, '-1')
  testToJs(-9, '~8')
})

test('op2', () => {
  testToJs(6, '2 *  3')
  testToJs(8, '2 ** 3')
  testToJs(2, '4 /  2')
  testToJs(1, '5 %  2')
  testToJs(3, '1 +  2')
  testToJs(1, '3 -  2')
  testToJs(4, '2 << 1')
  testToJs(1, '2 >> 1')
  testToJs(2, '6 &  3')
  testToJs(3, '2 |  1')
  testToJs(4, '7 ^  3')
  testToJs(true, '1 == 1')
  testToJs(false, '1 != 1')
  testToJs(false, '1 < 1')
  testToJs(true, '1 <= 1')
  testToJs(false, '1 > 1')
  testToJs(true, '1 >= 1')
  testToJs(false, 'false && true')
  testToJs(true, 'false || true')
})

test('variable with op2', () => {
  testToJs(1, 'var a = 6; a = 1')
  testToJs(7, 'var a = 6; a += 1')
  testToJs(5, 'var a = 6; a -= 1')
  testToJs(6, 'var a = 3; a *= 2')
  testToJs(3, 'var a = 6; a /= 2')
  testToJs(2, 'var a = 6; a %= 4')
  testToJs(2, 'var a = 6; a &= 3')
  testToJs(7, 'var a = 6; a |= 1')
  testToJs(4, 'var a = 7; a ^= 3')
  testToJs(8, 'var a = 4; a <<= 1')
  testToJs(2, 'var a = 4; a >>= 1')
})
