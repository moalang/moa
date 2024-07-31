#!node
'use strict'
// Executable file for bin/moa
const fs = require('fs')
const { parse } = require('./parse.js')
const { infer } = require('./infer.js')
const { compile } = require('./compile.js')

const src = fs.readFileSync('/dev/stdin', 'utf-8')
const runtime = fs.readFileSync(__dirname + '/runtime.js', 'utf-8')
if (process.argv[2] === 'js') {
  const ast = infer(parse(src))
  const js = runtime + '\n// ---\n\n' + compile(ast)
  console.log(js)
} else if (process.argv[2] === 'selftest') {
  const tests = src.replaceAll(/#.*/g, '').split(/(?=assert )/).map(s => s.trim()).filter(s => s)
  for (const test of tests) {
    let ast
    let js
    try {
      ast = parse(test)
      infer(ast)
      js = compile(ast)
      if (eval(runtime + js) === true) {
        process.stdout.write('.')
      } else {
        throw new Error('Assertion failure')
      }
    } catch (e) {
      console.dir(e, {depth: null})
      console.log(test, '  ->  ', js)
      process.exit(1)
    }
  }
  console.log('ok')
}
