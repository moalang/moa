const { parse } = require('./parser.js')
const { evaluate } = require('./interpriter.js')
const fs = require('fs')

const moa = fs.readFileSync(__dirname + '/moa.moa', 'utf8')
const iof = () => {
}
const __source = moa
const __write = js => {
  const code = `#!node
// this will be replaced native code after llvm ir backend implemented
const log = {
  debug: (...a) => console.dir(a, {depth: null}),
}
${js}
main({
  argv: process.argv.slice(2),
})
`
  fs.writeFileSync('a.out', code, 'utf8')
  fs.chmodSync('a.out', '0755')
}
const log = {
  debug: (...a) => console.dir(a)
}
evaluate(parse(moa + '\n__write(compile_to_js(__source))'), {__source, __write, log})
