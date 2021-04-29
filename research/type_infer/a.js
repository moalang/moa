// utils
const print = (...args) => console.log("|",...args)
const str = x => JSON.stringify(x)
const eq = (a,b) => a === b || str(a) === str(b)
const fail = (msg,o) => {
  process.stdout.write("\n-- failed\n")
  for(const k of Object.keys(o)) {
    process.stdout.write(k)
    process.stdout.write(" ")
    console.dir(o[k], {depth:null})
  }
  throw new Error(msg)
}

// nodes
const lambda = (arg,body) => ({tag:'lambda',arg,body,string:arg+" -> "+body.string})
const id = name => ({tag:'id',name,string:name})
const apply = (fn,arg) => ({tag:'apply',fn,arg,string:"("+fn.string+" "+arg.string+")"})
const letrec = (name,def,body) => ({tag:'let',name,def,body,string:body.string+" : "+name+" "+def.string})

let tvId = 0

// types
const to2 = (name,types) => ({name,types,string:[name].concat(types.map(t=>t.string)).join(" ")})
const to = (name,...types) => to2(name.string || name, types)
const it = to("I")
const bt = to("B")
const ut = to("U")
const ft = (...types) => ({name:'',types,string:types.map(t=>t.string).join(' ')})
const tv = () => ({string:"abcdefghijklmnopqrstuvwxyz"[tvId++].toString(),var:ut})

const analyse = (node, env) => {
  switch (node.tag) {
    case "id": return node.name.match(/^[0-9]+$/) ? it : env[node.name] || fail("not found",node.name)
    case "apply": {
      const f = analyse(node.fn, env)
      const at = analyse(node.arg, env)
      const rt = tv()
      unify(ft(at,rt),f)
      return rt
    }
    case "lambda": {
      const e = Object.assign({}, env)
      const at = e[node.arg] = tv()
      const rt = analyse(node.body, e)
      return ft(at,rt)
    }
    case "let": {
      const e = Object.assign({}, env)
      const nt = e[node.name] = tv()
      const dt = analyse(node.def,e)
      unify(nt,dt)
      return analyse(node.body,e)
    }
    default: fail("node", node.tag)
  }
}
const unify = (t1, t2) => {
  const a = prune(t1)
  const b = prune(t2)
  if (a.var) {
    if (!eq(a,b)) {
      a.var = b
    }
  } else if (!a.var && b.var) {
    unify(b,a)
  } else if (!a.var && !b.var) {
    if (a.name !== b.name) fail("missmatch name",{a,b})
    if (a.types.length !== b.types.length) fail("missmatch args",{a:a.types,b:b.types})
    a.types.map((t,i) => unify(t, b.types[i]))
  }
}
const prune = t => t.var && !eq(t.var, ut) ? t.var = prune(t.var) : t
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
