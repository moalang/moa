'use strict'
const assert = require('node:assert/strict')
const test = require('node:test').test
const vm = require('node:vm')

const { main, toJs, runtimeJs } = require('./moa.js')

function testToJs(expect, src) {
  const js = toJs(src)
  assert.deepEqual(vm.runInNewContext(runtimeJs + js), expect, `${src} -> ${js}`)
}

test('command line', () => {
  assert.match(main().out, /Usage:/)
  assert.deepEqual(true, vm.runInNewContext(main('to', ['js', 'true']).out))
})

test('compile literal', () => {
  testToJs(true, 'true')
  testToJs(false, 'false')
  testToJs(1, '1')
  testToJs(1.1, '1.1')
  testToJs('a', '"a"')
})
