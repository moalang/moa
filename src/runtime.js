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
  throw new Error('Match Error')
}
