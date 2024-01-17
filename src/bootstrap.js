const { exec } = require('child_process')
const { parse } = require('./parser.js')
const { evaluate } = require('./interpriter.js')
const { join } = require('node:path');
const fs = require('fs')

const moa = fs.readFileSync(__dirname + '/moa.moa', 'utf8')
const log = (...a) => (console.dir(a, {depth: null}), a[0])
const fail = m => { throw Error(m) }
const src = parse(moa + '\ncompile_to_go ' + JSON.stringify(moa))
const go = evaluate(src, {moa, fail, log}) + '\n'

const dir = join(__dirname, '..', 'bin')
const name = 'moa.go'
const path = join(dir, name)
fs.writeFileSync(path, go, 'utf8')
exec(`command go build ${name}`, {cwd: dir}, (err, stdout, stderr) => {
  console.log(stdout, stderr, err || '')
  //fs.unlinkSync(path)
})
