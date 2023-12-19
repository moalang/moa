const { parse } = require('./parser.js')
const { evaluate } = require('./interpriter.js')
const fs = require('fs')

const moa = fs.readFileSync(__dirname + '/moa.moa', 'utf8')
const __source = moa
const log = o => (console.dir(typeof o === 'function' ? o.toString() : o, {depth: null}), o)
const fail = m => { throw Error(m) }
const js = evaluate(parse(moa + 'compile_to_js(__source)'), {__source, fail, log})
const code = `#!node
// this will be replaced native code after llvm ir backend implemented
const log = o => (console.dir(o, {depth: null}), o)
${js}
main({argv: process.argv.slice(2)})
`
fs.writeFileSync('/tmp/a.out', code, 'utf8')
fs.chmodSync('/tmp/a.out', '0755')
