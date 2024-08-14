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

test('compile literal', () => {
  testToJs(true, 'true')
  testToJs(false, 'false')
  testToJs(1, '1')
  testToJs(-1, '-1')
  testToJs(1.1, '1.1')
  testToJs(1000, '1e3')
  testToJs(1200, '1.2e3')
  testToJs(255, '0xff')
  testToJs(1000, '1_000')
  testToJs('a', '"a"')
  testToJs('a"b', '"""a"b"""')
})
