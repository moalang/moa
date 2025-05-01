"use strict"

const log = x => { console.dir(x, {depth: null}); return x }

const testInfer = param => {
  const run = src => param.infer(param.parse(param.tokenize(src)))
  const showType = type => {
    const show = (t, mark) => t.instance ? show(t.instance, mark) :
      mark && t.mark ? (t.mark.includes("*") ? show(t.types[0]) + "*" : t.name + t.mark) :
      t.name === "fn" ? '(' + t.types.map((u, i) => show(u, i < t.types.length - 1)).join(' ') + ')' :
      t.name && t.types.length ? t.name + '[' + t.types.map(show).join(' ') + ']' :
      t.types.length ? '(' + t.types.map(show).join(' ') + ')' :
      t.name
    const s = show(type)
    const o = {}
    const r = s.replace(/\d+/g, t => o[t] ||= Object.keys(o).length + 1)
    return r
  }
  const reject = src => {
    try {
      run(src)
      throw new Error(src)
    } catch (e) {
      if (!(e instanceof param.TypeError)) {
        throw e
      }
    }
  }
  const inf = (expect, src) => {
    const actual = showType(run(src).at(-1).type)
    if (actual !== expect) {
      console.error(`${expect} != ${actual}\n${src}`)
      throw new Error(src)
    }
  }

  // primitives
  inf("int", "1")
  inf("bool", "true")
  inf("bool", "false")

  // unary operator
  inf("bool", "(! true)")
  inf("int", "(- 1)")

  // binary operator
  inf("int", "(+ 1 1)")
  inf("bool", "(< 1 1)")

  // branch
  inf("int", "(iif true 1 2)")
  inf("bool", "(iif true true true)")

  // function
  inf("(int int)", "(fn a (- a 1))")
  inf("(int int int)", "(fn a b (- a b 0))")
  inf("int", "((fn 1))")

  // generics
  inf("(1 1)", "(fn a a)")
  inf("(1 2 1)", "(fn a b a)")
  inf("(1 2 2)", "(fn a b b)")
  inf("int", "((fn a a) 1)")
  inf("bool", "(let f (fn a a))(f 1)(f true)")

  // container
  inf("vec[int]", "(vec 1)")

  // combinations
  inf("int",                           "(let f (fn x (+ x 1))) (let g (fn x (+ x 2))) (+ (f 1) (g 1))")
  inf("((1 2) (2 3) 1 3)",             "(fn f g x (g (f x)))")
  inf("((1 2 3) (1 2) 1 3)",           "(fn x y z (x z (y z)))")
  inf("(1 (1 bool) (1 1))",            "(fn b x (iif (x b) x (fn x b)))")
  inf("(bool bool)",                   "(fn x (iif true x (iif x true false)))")
  inf("(bool bool bool)",              "(fn x y (iif x x y))")
  inf("(1 1)",                         "(fn n ((fn x (x (fn y y))) (fn f (f n))))")
  inf("((1 2) 1 2)",                   "(fn x y (x y))")
  inf("((1 2) ((1 2) 1) 2)",           "(fn x y (x (y x)))")
  inf("(1 ((1 2 3) 4 2) (1 2 3) 4 3)", "(fn h t f x (f h (t f x)))")
  inf("((1 1 2) ((1 1 2) 1) 2)",       "(fn x y (x (y x) (y x)))")
  inf("(((1 1) 2) 2)",                 "(let id (fn x x)) (let f (fn y (id (y id))))")
  inf("(int)",                         "(let id (fn x x)) (let f (fn (iif (id true) (id 1) (id 2))))")
  inf("(int)",                         "(let f (fn x 1)) (let g (fn (+ (f true) (f 4))))")
  inf("(bool (1 1))",                  "(let f (fn x x)) (let g (fn y y)) (let h (fn b (iif b (f g) (g f))))")

  // variadic arguments
  inf("(int? int)", "(fn a? (+ a 0))")
  inf("int", "((fn a? (+ a 0)) 1)")
  inf("(int int? int)", "(fn a b? (+ a b 0))")
  inf("int", "((fn a b? (+ a b 0)) 1)")
  inf("int", "((fn a b? (+ a b 0)) 1 2)")
  inf("(1* vec[1])", "(fn a* a)")
  inf("vec[int]", "((fn a* a) 1)")
  inf("vec[int]", "((fn a* a) 1 2)")

  // type errors
  reject("(+ 1 true)")
  reject("(iif 1 2 3)")
  reject("(iif true 2 false)")
}

const testGenerate = param => {
  const test = (expected, exp, define="") => {
    const src = define.length ? define + "\n" + exp : exp
    const go = param.compile(src)
    const actual = param.runGo(go)
    if (expected !== actual) {
      throw new Error(`${expected} != ${actual}\n# src\n${src}\n# go\n${go.def}\n${go.stmt}\n${go.exp}`)
    }
  }

  // Value
  test("true", "true")
  test("1", "1")
  test("1.2", "1.2")
  test("hi", '"hi"')
  test("{1}", "(tuple 1)")
  test("{1 2}", "(tuple 1 2)")
  test("{1}", "(new a 1)")
  test("{1 2}", "(new a 1 b 2)")
  test("[1]", "(vec 1)")
  test("[1 2]", "(vec 1 2)")
  test("[true]", "(vec true)")
  test("map[1:true]", "(map 1 true)")
  test("map[1:true 2:false]", "(map 1 true 2 false)")
  test("map[1:{}]", "(set 1)")
  test("map[1:{} 2:{}]", "(set 1 2)")

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
  test("1", "(. (new a 1) a)")
  test("2", "(. (new a 1 b 2) b)")
  test("1", "((fn a (. a 0)) (tuple 1))")
  test("1", "((fn a (. a b)) (new b 1))")

  // Lambda
  test("true", "((fn true))")
  test("1", "((fn 1))")
  test("1.2", "((fn 1.2))")
  test("hi", '((fn "hi"))')
  test("1", '((fn a a) 1)')
  test("3", '((fn a b (+ a b)) 1 2)')

  // Definition
  test("1", "a", "(let a 1)")
  test("1", "(f)", "(let f (fn 1))")
  test("1", "(f)", "(let f (fn (do (return 1))))")
  test("1", "(f 1)", "(let f (fn a a))")
  test("3", "(f 1 2)", "(let f (fn a b (+ a b)))")
  test("{1}", "(a 1)", "(struct a () b int)")
  test("{1 true}", "(a 1 true)", "(struct a () b int c bool)")
  test("1", "(. (a 1) b)", "(struct a () b int)")
  test("false", "(. (a 1 false) c)", "(struct a () b int c bool)")
  test("1", "((fn c (. c b)) (a 1))", "(struct a () b int)")

  // Throw / Catch
  test("error: a", "(f)", '(let f (fn (do (throw "a"))))')
  test("a", '(catch (f) (fn x "b"))', '(let f (fn "a"))')
  test("c", '(catch (f) (fn x "c"))', '(let f (fn (do (throw "a") (return "b"))))')
  test("a", "(catch (f) (fn x (. x message)))", '(let f (fn (do (throw "a") (return "b"))))')
  test("a", '(iif false "" (catch (f) (fn x (. x message))))', '(let f (fn (do (throw "a") (return "b"))))')
  test("error: a", '(catch (iif false (f) (f)) (fn x (. x message)))', '(let f (fn (do (throw "a") (return "b"))))')

  // Statement
  test("1", "(iif true 1 2)")
  test("2", "(iif false 1 2)")
  test("2", "(iif false 1 true 2 3)")
  test("3", "(iif false 1 false 2 3)")
  test("1", "((fn (do (if true (return 1)) (return 2))))")
  test("2", "((fn (do (if false (return 1)) (return 2))))")
  test("2", "((fn (do (if false (return 1) (return 2)))))")
  test("2", "((fn (do (if false (return 1) true (return 2)) (return 3))))")
  test("6", "a", "(let a 0)(for b 4 (+= a b))")
  test("5", "a", "(let a 0)(for b 2 4 (+= a b))")
  test("9", "a", "(let a 0)(for b 1 6 2 (+= a b))")
  test("6", "a", "(let a 0)(each b (vec 1 2 3) (+= a b))")
  test("6", "a", "(let a 0)(while (< a 6) (+= a 1))")
  test("1", "a", "(let a 0)(for b 3 (do (+= a 1) break))")
  test("0", "a", "(let a 0)(for b 3 (do continue (+= a 1)))")

  // Generics
  test("1", "(iif (f true) (f 1) (f 2))", "(let f (fn a a))")
  test("{1}", "(a 1)", "(struct a (t) b t)")
  test("{1 true}", "(a 1 true)", "(struct a (t u) b t c u)")

  // IO
  test("hi2", '((. io put) "hi")')
}

module.exports.test = param => {
  console.clear()
  testInfer(param)
  const fs = require("fs")
  const {execSync} = require("child_process")
  const runtime = fs.readFileSync("runtime.go")
  const cache = fs.existsSync("/tmp/moa_test_cache.json") ? require("/tmp/moa_test_cache.json") : {}
  try {
    param.runGo = x => {
      const body = !x.fails ? `fmt.Print(${x.exp})` : `ret, err := ${x.exp}\nif err != nil { fmt.Print("error: " + err.Error()) } else { fmt.Print(ret) }`
      const go = `${runtime}\n${x.def}\nfunc main() {
        var __err error
        defer func() {
          if __err != nil {
            fmt.Print("error: " + __err.Error())
          }
        }()
        ${x.stmt}
        ${body}
      }\n`
      if (go in cache) {
        return cache[go]
      }
      fs.writeFileSync("/tmp/moa_test_exp.go", go)
      try {
        return cache[go] = execSync("go run /tmp/moa_test_exp.go").toString()
      } catch (e) {
        return `error: ${e}`
      }
    }
    testGenerate(param)
  } catch (e) {
    console.error(e.stack)
    return 1
  } finally {
    fs.writeFileSync("/tmp/moa_test_cache.json", JSON.stringify(cache, null, 2))
  }
}
