const __string = o => typeof o === 'string' ? o :
  o instanceof Array ? `(list ${o.map(__string).join(' ')})` :
  o instanceof Map ? `(dict ${[...o].map(__string).join(' ')})` :
  o instanceof Set ? `(dict ${[...o].map(__string).join(' ')})` :
  o.toString()
const __throw = (m, d) => { const e = new Error(m); e.detail = d; throw e }
const __dict_set = (m, k, v) => (m.set(k, v), v)
const assert = (a, b) => __string(a) === __string(b) ? process.stdout.write('.') : __throw(`\`${__string(a)}\` is not \`${__string(b)}\``, a, b)
const tuple = (...a) => a
const list = (...a) => a
const dict = (...a) => new Map([...Array(a.length/2)].map((_,i) => [a[i*2], a[i*2+1]]))
const log = (...a) => (console.log(...a.map(__string)), a.at(-1))
