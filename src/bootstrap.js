"use strict"

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

const compile = code => {
  const tokens = code.split(/([()]|[^ ()]+)/).filter(s => s.trim())
  const parse = (acc, [t, ...ts]) =>
    !t ? acc :
    t === ")" ? acc.concat(parse([], ts)) :
    t === "(" ? acc.concat([parse([], ts)]) :
    parse(acc.concat(t), ts)
  const string = x => Array.isArray(x) ? `(${x.map(string).join(" ")})` : x
  const genstmt = x => !Array.isArray(x) ? `return ${gen(x)}` : x.slice(0, -1).map(gen).join(";\n") + `return ${gen(x.at(-1))}`
  const gen = x => !Array.isArray(x) ? x :
    x[0] === "fn" ? `__Fn[func () int]{fn: func () int { ${genstmt(x.at(-1))} }, code: ${JSON.stringify(string(x))}}` :
    gen(x[0]) + "(" + x.slice(1).map(gen).join(", ") + ")"
  log(parse([], tokens))
  return parse([], tokens).map(gen).join("\n")
}

const testMain = () => {
  const crypto = require("crypto")
  const child_process = require("child_process")
  const fs = require("fs")

  const runtime = fs.readFileSync("runtime.go")
  const runGoExp = goExp => {
    fs.writeFileSync("/tmp/test_moa.go", `${runtime}\nfunc main() { fmt.Print(${goExp}) }\n`)
    try {
      return child_process.execSync("go run /tmp/test_moa.go").toString()
    } catch (e) {
      return `error: ${e}`
    }
  }
  const test = (expected, exp) => {
    const go = compile(exp)
    const actual = runGoExp(go)
    if (expected !== actual) {
      console.log(`${expected} != ${actual}\n# exp\n${exp}\n# go\n${go}`)
      process.exit(1)
    }
  }

  test("1", "1")
  test("1.2", "1.2")
  test("hi", '"hi"')
  test("(fn 1)", "(fn 1)")
  //test("(fn 1.2)", "(fn 1.2)")
}

console.clear()
testMain()
