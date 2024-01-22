'use strict'

const { exec } = require('child_process')
const { parse } = require('./parser.js')
const { evaluate } = require('./interpriter.js')
const { join } = require('node:path');
const fs = require('fs')

const escape = s => '"' + s.replace(/\\/g, '\\\\').replace(/"/g, '\\"').replace(/\n/g, '\\n') + '"'

const main = () => {
  const moa = fs.readFileSync(__dirname + '/moa.moa', 'utf8')
  const tmp = fs.readFileSync(__dirname + '/tmp.moa', 'utf8')
  const log = (...a) => (console.dir(a, {depth: null}), a[0])
  const fail = m => { throw Error(m) }
  const ast = parse(moa + '\ncompile_to_go ' + escape(tmp))
  const go_src = evaluate(ast, {moa, fail, log})
  const go_name = 'moa.go'
  const bin_dir = join(__dirname, '..', 'bin')
  const path = join(bin_dir, go_name)
  fs.writeFileSync(path, go_src, 'utf8')
  exec(`command go build ${go_name}`, {cwd: bin_dir}, (err, stdout, stderr) => {
    console.log(stdout, stderr, err || '')
    //fs.unlinkSync(path)
  })
}
try {
  main()
} catch (e) {
  console.log(e.message)
  e.detail && console.dir(e.detail, {depth: null})
}
