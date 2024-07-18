const __show = o => typeof o === 'string' ? o :
  o instanceof Array ? `(list ${o.map(__show).join(' ')})` :
  o instanceof Map ? `(dict ${[...o].map(__show).join(' ')})` :
  o instanceof Set ? `(dict ${[...o].map(__show).join(' ')})` :
  o.toString()
const __io_put = (...a) => process.stdout.write(a.map(__show).join(' '))
const __io_puts = (...a) => process.stdout.write(a.map(__show).join(' ') + '\n')
const __fail = (m, ...a) => { const e = new Error(m); e.detail = a; throw e }
const assert = (a, b) => __show(a) === __show(b) ? process.stdout.write('.') : __fail(`\`${__show(a)}\` is not \`${__show(b)}\``, a, b)
const list = (...a) => a
const dict = (...a) => new Map([...Array(a.length/2)].map((_,i) => [a[i*2], a[i*2+1]]))
const __dict_set = (m, k, v) => (m.set(k, v), v)
