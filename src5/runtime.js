// -- runtime
let io = {
  reads: () => '',
  write: s => console.log(s),
}
function __equal(a, b) {
  // JSON.stringify is needed to compare two objects
  return a === b || JSON.stringify(a) === JSON.stringify(b)
}
function __match(...matchers) {
  for (const matcher of matchers) {
    const ret = matcher()
    if (ret !== undefined) {
      return ret
    }
  }
  throw __error('Match Error ' + JSON.stringify(matchers))
}
function __guard(b) {
  return () => b ? {} : __error('guard')
}
function __stringInt(s) {
  return () => {
    const i = parseInt(s)
    if (isNaN(i)) {
      return __error('string.int: not a number ' + s)
    } else {
      return i
    }
  }
}
function __stringChar(s, n) {
  return () => 0 <= n && n < s.length ? s.slice(n, n+1) : __error('out of index')
}
function __arrayAt(a, n) {
  return () => 0 <= n && n < a.length ? a[n] : __error('out of index')
}
function __alt(eff, a) {
  return () => (r => r.__failed ? a : r)(eff())
}
function __then(eff, f) {
  return () => (r => r.__failed ? r : f(r))(eff())
}
function __error(message) {
  return {message, __failed: true}
}


// -- application
