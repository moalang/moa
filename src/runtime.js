function __equal(a, b) {
  return (a.__type && a.__type === b) || (JSON.stringify(a) === JSON.stringify(b))
}
