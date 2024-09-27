'use strict'

tester((eq, only) => {
  // Value
  eq(1, '1')
  eq(1.2, '1.2')
  eq('a', '"a"')
  eq(true, 'true')
  eq(false, 'false')
  eq(1, 'a', {a: 1})

  // Property
  eq(1, 'a.b()', {a: {b: () => 1}})
  eq(true, '"a".has("a")')
  eq(false, '"a".has("b")')

  // Method
  eq(1, 'a.b', {a: {b: 1}})
  eq(1, '"a".size')
  eq(1, '"1".int')
  eq(1.2, '"1.2".float')

  // Unry operator
  eq(false, '!true')
  eq(true, '!false')

  //Binary operator
  eq(true, 'true && true')
  eq(false, 'true && false')
  eq(false, 'false && true')
  eq(false, 'false && false')
  eq(true, 'true || true')
  eq(true, 'true || false')
  eq(true, 'false || true')
  eq(false, 'false || false')
  eq(true, '1 == 1')
  eq(false, '1 == 2')
  eq(false, '1 != 1')
  eq(true, '1 != 2')
  eq(false, '1 < 1')
  eq(true, '1 <= 1')
  eq(false, '1 > 1')
  eq(true, '1 >= 1')

  // Parenthesis
  eq(9, '(1 + 2) * 3')
  eq(1, '((1))')

  // Function
  eq(1, 'f()', {f: () => 1})
  eq(1, 'f(1)', {f: n => n})
  eq(1, 'def f: 1\nf()')
  eq(1, 'def f n: n\nf(1)')
  eq('a', 'def f n: n\nf("a")')

  // Test
  eq(true, 'test t: t.eq(1 1)')
  eq(new Error(`1: '1' != '2'`), 'test t: t.eq(1 2)')
  eq(new Error(`3: '1' != '2'`), 'test t:\n  t.eq(1 1)\n  t.eq(1 2)')

  // Complex expression
  eq(1, 'f(f(1))', {f: n => n})

  // moa.moa
  const fs = require('node:fs')
  const moa = fs.readFileSync('moa.moa', 'utf-8')
  eq(true, moa)
})

function tester(f) {
  const test = require('node:test')
  const assert = require('node:assert').strict
  const {run} = require('./bootstrap')
  function summary(s) {
    return s.includes('\n') ? s.split('\n')[0] + '...' : s
  }
  function noop() {}
  function eq(expected, source, env) {
    const line = new Error().stack.split('\n')[2].split(':').at(-2)
    if (expected instanceof Error) {
      test(`${line}: Error(${expected.message}) == ${summary(source)}`, () => {
        try {
          run(source, env)
          assert.fail('no exception')
        } catch (e) {
          assert.equal(e.message, expected.message, source)
        }
      })
    } else {
      test(`${line}: ${expected} == ${summary(source)}`, () => {
        assert.equal(run(source, env), expected, source)
      })
    }
  }
  if (/\n *only\(/.test(f.toString())) {
    f(noop, eq)
  } else {
    f(eq, noop)
  }
}
