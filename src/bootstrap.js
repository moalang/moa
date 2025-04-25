"use strict"

// - [x] bool literal
// - [x] int literal
// - [x] float literal
// - [x] string literal
// - [-] fn
// - [ ] tuple
// - [ ] struct
// - [ ] vec
// - [ ] set
// - [ ] map
// - [ ] let
// - [ ] type

const log = x => { console.dir(x, {depth: null}); return x }

const show = x => Array.isArray(x) ? `(${x.map(show).join(" ")})` : x.code

const tokenize = code => code.split(/([()]|[^ ()]+)/).filter(s => s.trim()).map(s => ({code: s}))

const parse = (acc, [t, ...ts]) =>
  !t ? acc :
  t.code === ")" ? acc.concat(parse([], ts)) :
  t.code === "(" ? acc.concat([parse([], ts)]) :
  parse(acc.concat(t), ts)

const infer = root => {
  const newtype = (name, args=[]) => ({name, args})
  const tbool = newtype("bool")
  const tint = newtype("int")
  const tfloat = newtype("float64")
  const tstring = newtype("tstring")
  const inferWith = (top, local, ng) => {
    const _inf = node => {
      if (Array.isArray(node)) {
        const head = node[0]
        const tail = node.slice(1)
        if (Array.isArray(head)) {
        } else {
          if (head.code === "fn") {
            return head.type = newtype("fn", tail.map(inf))
          } else {
            throw new Error("unknown")
          }
        }
      } else {
        const s = node.code
        return /^[0-9]+\./.test(s) ? tfloat :
          /^[0-9]+/.test(s) ? tint :
          s === "true" || s === "false" ? tbool :
          s.startsWith('"') ? tstring :
          local[s] || `UNKNOWN(${s})`
      }
    }
    const inf = node => node.type = _inf(node)
    return inf(top)
  }

  const env = {}
  root.map(node => inferWith(node, env, []))
  log(root.map(show).join("\n"))
  return root
}

const generate = root => {
  const genstmt = x => !Array.isArray(x) ? `return ${gen(x)}` : x.slice(0, -1).map(gen).join(";\n") + `return ${gen(x.at(-1))}`
  const gentype = t => t.name === "fn" ? `func (${t.args.slice(0, -1).map(gentype).join(", ")}) ${gentype(t.args.at(-1))}` : t.args.length ? `${t.name}[${t.args.map(gentype)}` : t.name
  const gen = x => !Array.isArray(x) ? x.code :
    x[0].code === "fn" ? `__Fn[${gentype(x.type)}]{fn: ${gentype(x.type)} { ${genstmt(x.at(-1))} }, code: ${JSON.stringify(show(x))}}` :
    gen(x[0]) + "(" + x.slice(1).map(gen).join(", ") + ")"
  return gen(root)
}

const compile = code => infer(parse([], tokenize(code))).map(generate).join("\n")

const testMain = runGoExp => {
  const test = (expected, exp) => {
    const goExp = compile(exp)
    const actual = runGoExp(goExp)
    if (expected !== actual) {
      throw new Error(`${expected} != ${actual}\n# exp\n${exp}\n# go\n${goExp}`)
    }
  }

  test("true", "true")
  test("1", "1")
  test("1.2", "1.2")
  test("hi", '"hi"')
  test("(fn 1)", "(fn 1)")
  test("(fn 1.2)", "(fn 1.2)")
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
    console.error(e.message)
    return 1
  } finally {
    fs.writeFileSync("/tmp/moa_test_cache.json", JSON.stringify(cache, null, 2))
  }
}

console.clear()
process.exit(main())
