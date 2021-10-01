// utils
const print = (...args) => console.log(...args)
const str = o => JSON.stringify(o)
const eq = (x,y) => str(x) === str(y)
const fail = (msg,...args) => {
  args.map(a => console.log("-", a))
  throw new Error(msg)
}

// nodes
const id = name => ({tag:'id',name,string:name})
const lambda = (arg,body) => ({tag:'lambda',arg,body,string:`${arg} -> ${body.string}`})
const apply = (fn,arg) => ({tag:'apply',fn,arg,string:`(${fn.string} ${arg.string})`})
const letrec = (name,def,body) => ({tag:'let',name,def,body,string:`${body.string} : ${name} ${def.string}`})

// types
let tvId = 0
const tv = () => (name => ({name,var:ut,string:name}))("abcdefghijklmn"[tvId++])
const to = (name,...types) => ({name,types,string:[name].concat(types.map(t=>t.string)).join(" ")})
const ut = to("U")
const it = to("I")
const bt = to("B")
const ft = (...types) => ({name:'',types,string:types.map(t=>t.string).join(" ")})

// type inference
const analyse = (n,env) => {
  switch (n.tag) {
    case "id": return n.name.match(/^[0-9]+$/) ? it : env[n.name] || fail("not found", n)
    case "apply": {
      const fn = analyse(n.fn,env)
      const arg = analyse(n.arg,env)
      const t = tv()
      unify(ft(arg,t),fn)
      return t
    }
    case "lambda": {
      const e = Object.assign({},env)
      const t = env[n.arg] = tv()
      return ft(t,analyse(n.body,e))
    }
    case "let": {
      const e = Object.assign({},env)
      const t = e[n.name] = tv()
      unify(t,analyse(n.def,e))
      return analyse(n.body,e)
    }
    default: fail("analyse",n)
  }
}
const unify = (a,b) => {
  a = prune(a)
  b = prune(b)
  if (a.var) {
    if (!eq(a,b)) {
      a.var = b
    }
  } else if (!a.var, b.var) {
    unify(b,a)
  } else if (!a.var && !b.var) {
    if (a.name !== b.name) { fail("name", a, b) }
    if (a.types.length !== b.types.length) { fail("types", a, b) }
    a.types.map((t,i) => unify(t,b.types[i]))
  }
}
const prune = t => t.var && !eq(t.var,ut) ? t.var = prune(t.var) : t

// main
function main() {
  const v1 = tv()
  const env = {
    true: bt,
    false: bt,
    if: ft(bt,ft(v1,ft(v1,v1))),
    prev: ft(it,it),
    zero: ft(it,bt),
    times: ft(it,ft(it,it)),
  }
  const list = [
    id("5"),
    lambda("n",id("5")),
    lambda("n",id("n")),
    lambda("n", lambda("m", id("5"))), // n -> m -> 5 : a->b->int
    letrec("dec", lambda("n", apply(id("prev"), id("n"))), // fun dec(n) = n - 1; dec(1)
      apply(id("dec"), id("1"))),
    letrec("factorial",   // fun factorial(n) = if zero(n) 1 else n * factorial(n-1): int
      lambda("n",
        apply(apply(apply(id("if"),
          apply(id("zero"),id("n"))),
          id("1")),
          apply(apply(id("times"),id("n")),apply(id("prev"),id("n"))))),
      apply(id("factorial"), id("5")))
  ]
  list.map(v => {
    console.log(prune(analyse(v,env)).string,"\t|",v.string)
  })
}
main()
