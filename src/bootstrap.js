const { exec } = require('child_process')
const { parse } = require('./parser.js')
const { evaluate } = require('./interpriter.js')
const fs = require('fs')

const moa = fs.readFileSync(__dirname + '/moa.moa', 'utf8')
const log = (...a) => (console.dir(a, {depth: null}), a[0])
const fail = m => { throw Error(m) }
const node = parse(moa + '\ncompile_to_js ' + JSON.stringify(moa))
const js = evaluate(node, {moa, fail, log})

const code = `#!node
// this will be replaced native code after llvm ir backend implemented
const log = o => (console.dir(o, {depth: null}), o)
${js}
main({argv: process.argv.slice(2), write: (...a) => console.log(...a)})
`
fs.writeFileSync('/tmp/moa', code, 'utf8')
fs.chmodSync('/tmp/moa', '0755')
exec('node /tmp/moa', (err, stdout, stderr) => console.log(stdout, stderr, err || ''))
