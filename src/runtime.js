const range = n => [...new Array(n)].map((_, i) => i)
const tuple = (...a) => (a.__tuple=true, a)
const vec = (...a) => a
const map = (...a) => new Map(range(a.length/2).map(i => [a[i*2], a[i*2+1]]))
const set = (...a) => new Set(a)

export {
  tuple,
  vec,
  set,
  map,
}
