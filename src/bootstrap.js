"use strict"

// - [x] bool literal
// - [x] int literal
// - [x] float literal
// - [x] string literal
// - [x] fn
// - [x] tuple
// - [x] struct
// - [ ] vec
// - [ ] set
// - [ ] map
// - [x] do
// - [ ] let
// - [ ] type

const log = x => { console.dir(x, {depth: null}); return x }

const show = x => Array.isArray(x) ? `(${x.map(show).join(" ")})` : x.code
const range = n => [...new Array(n)].map((_, i) => i)

const tokenize = code => code.split(/([()]|[^ ()]+)/).filter(s => s.trim()).map(s => ({code: s}))

const parse = ([t, ...ts], acc=[]) =>
  !t ? acc :
  t.code === ")" ? [acc, ts] :
  t.code === "(" ? (([x, rest]) => parse(rest, acc.concat([x])))(parse(ts)) :
  parse(ts, acc.concat(t))

const infer = root => {
  const prune = x => x.instance ? x.instance = prune(x.instance) : x
  const newtype = (name, args=[]) => ({name, args})
  const newfn = args => newtype("fn", args)
  const newtuple = args => newtype("tuple", args)
  const newstruct = (fields, args) => ({...newtype("struct__" + fields.join("__"), args), fields})
  let varId = 0
  const newvar = () => newtype(varId++)
  const tv1 = newvar()
  const tvoid = newtype("")
  const tbool = newtype("bool")
  const tint = newtype("int")
  const tfloat = newtype("float64")
  const tstring = newtype("string")
  const method = (t, field) => {
    if (t.fields) {
      return t.args[t.fields.indexOf(field)]
    } else {
      throw new Error(`No ${field} in ${t.name}`)
    }
  }
  const inferWith = (top, local, ng) => {
    const unify = (a, b) => {
      if (typeof a.name === "number") {
        a.instance = b
      } else if (typeof b.name === "number") {
        unify(b, a)
      } else {
        if (a.name !== b.name) {
          throw new Error(`Type name ${a.name} != ${b.name}`)
        }
        if (a.args.length !== b.args.length) {
          throw new Error(`Type args ${a.args} != ${b.args}`)
        }
        a.args.map((x, i) => unify(x, b[i]))
      }
    }
    const apply = (f, args) => {
      if ((f.args.length - 1) !== args.length) {
        throw new Error(`Calling ${f.args.length - 1} != ${args.length}`)
      }
      args.map((x, i) => unify(x, f.args[i]))
      return f.args.at(-1)
    }
    const iscmp = s => "== != < > >= <=".split(" ").includes(s)
    const islogical = s => s === "||" || s === "&&"
    const _inf = node => {
      if (Array.isArray(node)) {
        const head = node[0]
        const tail = node.slice(1)
        if (Array.isArray(head)) {
          return apply(inf(head), tail.map(inf))
        } else {
          const s = head.code
          if (s === "fn") {
            const args = tail.slice(0, -1).map(t => [t.code, t.type = newvar()])
            const body = inferWith(tail.at(-1), {...local, ...Object.fromEntries(args)}, ng)
            return newfn([...args.map(a => a[1]), body])
          } else if (s === "do") {
            const ret = tail.find(x => x[0].code === "return")
            tail.map(inf)
            return ret ? ret[1].type : tvoid
          } else if (s === "tuple") {
            return newtuple(tail.map(inf))
          } else if (s === "struct") {
            const fields = range(tail.length / 2).map(i => tail[i*2].code)
            const values = range(tail.length / 2).map(i => inf(tail[i*2+1]))
            return newstruct(fields, values)
          } else if (islogical(s)) {
            tail.map(inf).map(t => unify(tbool, t))
            return tbool
          } else if ("+-*/%^=".includes(s) || iscmp(s)) {
            const [t, ...ts] = tail.map(inf)
            ts.map(x => unify(t, x))
            newfn(range(tail.length + 1).map(_ => t))
            return iscmp(s) ? tbool : t
          } else if (s === ".") {
            const target = inf(tail[0])
            if (target.name === "tuple") {
              return target.args[tail[0].code]
            } else {
              const field = tail[1].code
              const args = tail.slice(2).map(inf)
              return method(target, field)
            }
          } else {
            return apply(inf(head), tail.map(inf))
          }
        }
      } else {
        const s = node.code
        const t = /^[0-9]+\./.test(s) ? tfloat :
          /^[0-9]+/.test(s) ? tint :
          s === "true" || s === "false" ? tbool :
          s.startsWith('"') ? tstring :
          local[s] || env[s]
        if (!t) {
          throw new Error(`Typing fails to '${s}'`)
        }
        return t
      }
    }
    const inf = node => node.type = _inf(node)
    return inf(top)
  }
  const fix = node => {
    if (Array.isArray(node)) {
      node.map(fix)
    }
    if (node.type) {
      node.type = prune(node.type)
      node.type.args = node.type.args.map(prune)
    }
  }

  const env = {
    "!": newfn([tbool, tbool]),
    "return": newfn([tv1, tv1])
  }
  root.map(node => inferWith(node, env, []))
  root.map(fix)
  return root
}

const generate = root => {
  const genreturn = x => x[0]?.code === "do" ? gen(x) : `return ${gen(x)}`
  const genop = (op, xs) => xs.length === 1 ? `${op} ${gen(xs[0])}` : genop2(op, xs)
  const genop2 = (op, xs) => xs.length === 1 ? gen(xs[0]) : `${gen(xs[0])} ${op} ${genop2(op, xs.slice(1))}`
  const gentype = t => t.args.length ? `${t.name}[${t.args.map(gentype)}` : t.name
  const gen = x => !Array.isArray(x) ? x.code :
    x[0].code === "fn" ? `func (${x.slice(1, -1).map(a => `${a.code} ${gentype(a.type)}`)}) ${gentype(x.at(-1).type)} { ${genreturn(x.at(-1))} }` :
    x[0].code === "do" ? x.slice(1).map(gen).join("\n") :
    x[0].code === "tuple" ? `__tuple${x.length - 1}[${x.slice(1).map(node => gentype(node.type))}]{ ${x.slice(1).map(gen)} }` :
    x[0].code === "struct" ? `struct { ${x.type.name.split("__").slice(1).map((field, i) => `${field} ${gentype(x.type.args[i])}`).join("\n")} }{${range((x.length-1)/2).map(i => gen(x[i*2+2]))}}` :
    x[0].code === "!" ? `${x[0].code} ${gen(x[1])}` :
    x[0].code === "." && x[1]?.type?.name === "tuple" ? `${gen(x[1])}.v${x[2].code}` :
    /[+\-*/%^<>!=|&.]/.test(x[0].code) ? genop(x[0].code, x.slice(1)) :
    gen(x[0]) + "(" + x.slice(1).map(gen).join(", ") + ")"
  return root.map(gen).join(";\n")
}

const compile = code => generate(infer(parse(tokenize(code))))

const testMain = runGoExp => {
  const test = (expected, exp) => {
    const goExp = compile(exp)
    const actual = runGoExp(goExp)
    if (expected !== actual) {
      throw new Error(`${expected} != ${actual}\n# exp\n${exp}\n# go\n${goExp}`)
    }
  }

  // Value
  test("true", "true")
  test("1", "1")
  test("1.2", "1.2")
  test("hi", '"hi"')
  test("{1}", "(tuple 1)")
  test("{1 2}", "(tuple 1 2)")
  test("{1}", "(struct a 1)")
  test("{1 2}", "(struct a 1 b 2)")

  // Operator
  test("false", "(! true)")
  test("-1", "(- 1)")
  test("-2", "(^ 1)")
  test("3", "(+ 1 2)")
  test("1", "(- 3 2)")
  test("6", "(* 2 3)")
  test("2", "(/ 4 2)")
  test("1", "(% 3 2)")
  test("3", "(^ 1 2)")
  test("true", "(== 1 1)")
  test("false", "(!= 1 1)")
  test("false", "(< 1 1)")
  test("false", "(> 1 1)")
  test("true", "(<= 1 1)")
  test("true", "(>= 1 1)")
  test("6", "(+ 1 2 3)")

  // Method
  test("1", "(. (tuple 1) 0)")
  test("2", "(. (tuple 1 2) 1)")
  test("1", "(. (struct a 1) a)")
  test("2", "(. (struct a 1 b 2) b)")

  // Lambda
  test("true", "((fn true))")
  test("1", "((fn 1))")
  test("1.2", "((fn 1.2))")
  test("hi", '((fn "hi"))')
  test("1", '((fn a a) 1)')
  test("3", '((fn a b (+ a b)) 1 2)')

  // Statement
  test("1", '((fn a (do (return a))) 1)')
  test("2", '((fn a b (do (return b))) 1 2)')

  // Container
}

const main = () => {
  const fs = require("fs")
  const child_process = require("child_process")
  const runtime = fs.readFileSync("runtime.go")
  const safe = (f, g) => { try { return f() } catch(e) { return g(e) } }
  const cache = safe(() => require("/tmp/moa_test_cache.json"), () => ({}))
  try {
    const runGoExp = goExp => {
      const go = `${runtime}\nfunc main() { fmt.Print(${goExp}) }\n`
      if (go in cache) {
        return cache[go]
      }
      fs.writeFileSync("/tmp/moa_test_exp.go", go)
      return cache[go] = safe(() => child_process.execSync("go run /tmp/moa_test_exp.go").toString(), e => `error: ${e}`)
    }
    testMain(runGoExp)
  } catch (e) {
    console.error(e.stack)
    return 1
  } finally {
    fs.writeFileSync("/tmp/moa_test_cache.json", JSON.stringify(cache, null, 2))
  }
}

console.clear()
process.exit(main())
