const {execSync} = require('child_process');
const write = s => process.stdout.write(s)
const print = (...a) => console.log(...a)
function test(lang, run, expect, exp, ...defs) {
  const src = defs.concat(['main='+exp]).join('\n')
  const dst = execSync('node bootstrap.js ' + lang, {input: src}).toString()
  try {
    const stdout = run(dst)
    if (stdout === expect) {
      write('.')
    } else {
      print('src   :', src)
      print('expect:', expect)
      print('stdout:', stdout)
      print('dst   :', dst)
      process.exit(1)
    }
  } catch (e) {
    print('src :', src)
    print('dst:', dst)
    throw e
  }
}
function llvm(...args) {
  test('', llvm => execSync('lli', {input: llvm}).toString(), ...args)
}
function js(...args) {
  test('js', js => execSync('node', {input: js}).toString(), ...args)
}
llvm('', '1')
js('1', 'io.write(1)')
print('ok')
