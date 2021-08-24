const {execSync} = require('child_process');
const write = s => process.stdout.write(s)
const print = (...a) => console.log(...a)
function t(expect, exp, ...defs) {
  const src = defs.concat(['main='+exp]).join('\n')
  const llvm = execSync('node bootstrap.js', {input: src}).toString()
  const stdout = execSync('lli', {input: llvm}).toString()
  if (stdout === expect) {
    write('.')
  } else {
    print('src   :', src)
    print('expect:', expect)
    print('stdout:', stdout)
    print('llvm  :', llvm)
    process.exit(1)
  }
}
t('', 'io.exit(0)')
print('ok')
