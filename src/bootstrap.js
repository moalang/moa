const { parse } = require('./parser.js')
const { evaluate } = require('./interpriter.js')
const fs = require('fs')

const moa = fs.readFileSync(__dirname + '/moa.moa', 'utf8')
const puts = o => console.log(o)
const __source = moa
const log = o => (console.dir(o), o)
const js = evaluate(parse(moa + 'compile_to_js(__source)'), {__source, log})
const code = `#!node
// this will be replaced native code after llvm ir backend implemented
const log = o => (console.dir(o, {depth: null}), o)
${js}
main({argv: process.argv.slice(2)})
`
fs.writeFileSync('a.out', code, 'utf8')
fs.chmodSync('a.out', '0755')
puts('ok')
