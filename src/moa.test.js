'use strict'
const assert = require('node:assert/strict')
const test = require('node:test').test
const vm = require('node:vm')

const { main, toJs, runtimeJs } = require('./moa.js')

function run(expect, src) {
  const js = toJs(src)
  const actual = vm.runInNewContext(runtimeJs + js)
  assert.deepEqual(actual, expect, `${src} -> ${js} -> ${actual}`)
}

test('command line', () => {
  assert.match(main().out, /Usage:/)
  assert.deepEqual(true, vm.runInNewContext(main('to', ['js', 'true']).out))
})

test('literal', () => {
  run(true, 'true')
  run(false, 'false')
  run(1, '1')
  run(1.1, '1.1')
  run(1000, '1e3')
  run(1200, '1.2e3')
  run(255, '0xff')
  run(1000, '1_000')
  run('a', '"a"')
  run('a"b', '"""a"b"""')
})

test('op1', () => {
  run(false, '!true')
  run(-1, '-1')
  run(-9, '~8')
})

test('op2', () => {
  run(6, '2 *  3')
  run(8, '2 ** 3')
  run(2, '4 /  2')
  run(1, '5 %  2')
  run(3, '1 +  2')
  run(1, '3 -  2')
  run(4, '2 << 1')
  run(1, '2 >> 1')
  run(2, '6 &  3')
  run(3, '2 |  1')
  run(4, '7 ^  3')
  run(true, '1 == 1')
  run(false, '1 != 1')
  run(false, '1 < 1')
  run(true, '1 <= 1')
  run(false, '1 > 1')
  run(true, '1 >= 1')
  run(false, 'false && true')
  run(true, 'false || true')
})

test('variable with op2', () => {
  run(1, 'var a = 6; a = 1')
  run(7, 'var a = 6; a += 1')
  run(5, 'var a = 6; a -= 1')
  run(6, 'var a = 3; a *= 2')
  run(3, 'var a = 6; a /= 2')
  run(2, 'var a = 6; a %= 4')
  run(2, 'var a = 6; a &= 3')
  run(7, 'var a = 6; a |= 1')
  run(4, 'var a = 7; a ^= 3')
  run(8, 'var a = 4; a <<= 1')
  run(2, 'var a = 4; a >>= 1')
})
