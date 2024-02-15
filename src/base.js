const __show = o => typeof o === 'string' ? o :
  o instanceof Array ? `(list ${o.map(__show).join(' ')})` :
  o instanceof Map ? `(dict ${[...o].map(__show).join(' ')})` :
  o instanceof Set ? `(dict ${[...o].map(__show).join(' ')})` :
  o.toString()
const __io_put = (...a) => process.stdout.write(a.map(__show).join(' '))
const __io_puts = (...a) => process.stdout.write(a.map(__show).join(' ') + '\n')
