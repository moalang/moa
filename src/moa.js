#!node
'use strict'
// Executable file for bin/moa
const fs = require('fs')
const {parse} = require('./parse.js')
const {infer} = require('./infer.js')
const {compile} = require('./compile.js')

const src = fs.readFileSync('/dev/stdin', 'utf-8')
const runtime = fs.readFileSync(__dirname + '/runtime.js', 'utf-8')
const ast = infer(parse(src))
const js = runtime + '\n// ---\n\n' + compile(ast)
if (process.argv[2] === 'js') {
  console.log(js)
} else {
  eval(js)
}
