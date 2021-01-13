function __equal(a, b) {
  // JSON.stringify is needed to compare two objects
  return a === b || JSON.stringify(a) === JSON.stringify(b)
}
