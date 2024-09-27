'use strict'

const test = require('node:test')
const assert = require('node:assert').strict
const {run} = require('./bootstrap')

tester((eq, only) => {
  // Value
  eq('1', 1)
  eq('1.2', 1.2)
  eq('"a"', 'a')
  eq('true', true)
  eq('false', false)
  eq('a', 1, {a: 1})

  // Function
  eq('f()', 1, {f: () => 1})
  eq('f(1)', 1, {f: n => n})
  eq('def f: 1\nf()', 1)
  eq('def f n: n\nf(1)', 1)

  // Property
  eq('a.b', 1, {a: {b: 1}})
  eq('"a".size', 1)

  // Unry operator
  eq('!true', false)
  eq('!false', true)

  //Binary operator
  eq('true && true', true)
  eq('true || true', true)
  eq('true == true', true)
  eq('true != true', false)
  eq('1 < 1', false)
  eq('1 <= 1', true)
  eq('1 > 1', false)
  eq('1 >= 1', true)

  // Parenthesis
  eq('(1 + 2) * 3', 9)
  eq('((1))', 1)
})

function tester(f) {
  function noop() {}
  function eq(source, expectation, env) {
    test(source + ' == ' + expectation, () => assert.equal(run(source, env), expectation, source))
  }
  if (/\n *only\(/.test(f.toString())) {
    f(noop, eq)
  } else {
    f(eq, noop)
  }
}
