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
  throw new Error('Match Error ' + JSON.stringify(matchers))
}
function __stringInt(m) {
  const i = parseInt(m)
  if (isNaN(i)) {
    return new Error('string.int: ' + m.toString() + ' is not a number')
  } else {
    return i
  }
}
