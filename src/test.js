const {execSync} = require('child_process');
const write = s => process.stdout.write(s)
const print = (...a) => console.log(...a)
function t(expect, exp, ...defs) {
  const src = defs.concat(['main='+exp]).join('\n')
  const llvm = execSync('node bootstrap.js', {input: src}).toString()
  try {
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
  } catch (e) {
    print('src :', src)
    print('llvm:', llvm)
    throw e
  }
}
t('', '1')
print('ok')
