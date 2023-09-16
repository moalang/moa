const { parse } = require('./parser.js')
const { evaluate, buildin } = require('./interpriter.js')
const fs = require('fs')

const moa = fs.readFileSync(__dirname + '/moa.moa', 'utf8')
console.log(moa)
evaluate(parse(moa), buildin)
