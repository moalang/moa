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
  throw new __error('Match Error ' + JSON.stringify(matchers))
}
function __stringInt(m) {
  return () => {
    const i = parseInt(m)
    if (isNaN(i)) {
      return new __error('string.int: not a number ' + m.toString())
    } else {
      return i
    }
  }
}
function __alt(eff, a) {
  return () => (r => r.__failed ? a : r)(eff())
}
function __then(eff, f) {
  return () => (r => r.__failed ? r : f(r))(eff())
}
function __error(message) {
  this.message = message
  this.__failed = true
}


// -- application
