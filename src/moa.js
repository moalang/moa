#!node
'use strict'

const { readFileSync } = require('fs')
const { parse } = require('./parse.js')
const { infer } = require('./infer.js')
const { compile } = require('./compile.js')

if (process.argv[2] === 'to' && process.argv[3] === 'js') {
  const src = readFileSync('/dev/stdin', 'utf-8')
  const runtime = readFileSync(__dirname + '/runtime.js', 'utf-8')
  const ast = infer(parse(src))
  const js = runtime + '\n// ---\n\n' + compile(ast)
  console.log(js)
} else {
  console.log(`Usage:
    moa                       # launch interactive shell
    moa env [+/-] [<version>] # list versions; use, install or remove a version
    moa ide [<port>]          # launch web IDE
    moa to [<language>]       # compile to a programming language`)
}
