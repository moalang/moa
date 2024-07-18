const ___string = o => typeof o === 'string' ? o :
  o instanceof Array ? `(list ${o.map(___string).join(' ')})` :
  o instanceof Map ? `(dict ${[...o].map(___string).join(' ')})` :
  o instanceof Set ? `(dict ${[...o].map(___string).join(' ')})` :
  o.toString()
const ___throw = (m, d) => { const e = new Error(m); e.detail = d; throw e }
const ___dict_set = (m, k, v) => (m.set(k, v), v)
const ___assert = (a, b) => ___string(a) === ___string(b) ? process.stdout.write('.') : __throw(`\`${___string(a)}\` is not \`${___string(b)}\``, a, b)
const ___tuple = (...a) => a
const ___list = (...a) => a
const ___dict = (...a) => new Map([...Array(a.length/2)].map((_,i) => [a[i*2], a[i*2+1]]))
const ___log = (...a) => (console.log(...a.map(___string)), a.at(-1))
