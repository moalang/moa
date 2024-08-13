const assert = require('node:assert')
const { test } = require('node:test')
const { readFileSync } = require('node:fs')
const path = require('node:path')

const { parse } = require('./parse')
const { infer } = require('./infer')
const { compile } = require('./compile')

const runtime = readFileSync(__dirname + '/runtime.js', 'utf-8')
const code = readFileSync(path.join(__dirname, '../test/core.moa'), 'utf-8')
const tests = code.replaceAll(/#.*/g, '').split(/(?=assert )/).map(s => s.trim()).filter(s => s)

test('core test', () => {
  for (const test of tests.slice(0, 34)) {
    const ast = parse(test)
    infer(ast)
    const js = compile(ast)
    assert.equal(true, eval(runtime + js), {ast, js})
  }
})
