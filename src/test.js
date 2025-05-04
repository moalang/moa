"use strict"

const log = x => { console.dir(x, {depth: null}); return x }

const testSugar = param => {
  const show = o => Array.isArray(o) ? `(${o.map(show).join(" ")})` : o.code
  const test = (expect, src) => {
    const actual = show(param.sugar(src, "test.moa"))
    if (actual !== expect) {
      console.error(`${expect} != ${actual}\n${src}`)
      throw new Error(src)
    }
  }
  // basic
  test("a", "a")
  test("(a b)", "a b")
  test("(a)", "a()")
  test("(a b)", "a(b)")
  test("(! true)", "!true")
  test("(+ 1 2)", "1 + 2")
  test("(+ 1 (+ 2 3))", "1 + 2 + 3")
  test("(. a b)", "a.b")
  test("((. a b) c)", "a.b c")
  test("(fn a a)", "a => a")
  test("(fn a b (+ a b))", "a,b => a + b")
  test("(fn a (do a (return (b c))))", "a =>\n  a\n  b c")
  test("(a b)", "a: b")
  test("(a (b c))", "a: b c")
  test("(a b c)", "a b: c")
  test("(a (do b (c d)))", "a :\n  b\n  c d")

  // combination
  test("((. a b) (fn c d))", "a.b c => d")
  test("(+ (. a b) c)", "a.b + c")
  test("(+ a (. b c))", "a + b.c")
  test("(a (b c))", "a:\n  b:\n    c")
  test("(a (do b (c d)))", "a:\n  b\n  c:\n    d")
  test("(a (do b (c (do d e))))", "a:\n  b\n  c:\n    d\n    e")
  test("(fn a (fn b c))", "a =>\n  b =>\n    c")
  test("(fn a (do b (return (fn c d))))", "a =>\n  b\n  c =>\n    d")
  test("(fn a (do b (return (fn c (do d (return e))))))", "a =>\n  b\n  c =>\n    d\n    e")

  // variable arguments
  test("(+ 1 (+ 2 3))", "+ 1 2 3")
  test("(+ 1 (+ 2 3))", "(+ 1 2 3)")
  test("(a (+ 1 (+ 2 3)))", "(a (+ 1 2 3))")
  test("(iif a b c)", "iif a b c")
  test("(iif a b (iif c d e))", "iif a b c d e")
  test("(if a (if b c d))", "if a b c d")
  test("(if a b (if c d e))", "if a b c d e")
  test("(if a b (if c (if d e f)))", "if a b c d e f")
  test("(if a b (if c d (if e f g)))", "if a b c d e f g")
  test("(for _ 0 3 1 a)", "for 3 a")
  test("(for i 0 3 1 a)", "for i 3 a")
  test("(for i 1 3 1 a)", "for i 1 3 a")
  test("(for i 1 3 2 a)", "for i 1 3 2 a")
  test("(each _ a b)", "each a b")
  test("(each a b c)", "each a b c")
}

const testInfer = param => {
  const run = src => param.infer(param.parse(src))
  const showType = type => {
    const show = (t, mark) => t.instance ? show(t.instance, mark) :
      mark && t.mark ? (t.mark.includes("*") ? show(t.types[0]) + "*" : t.name + t.mark) :
      t.name === "fn" ? '(' + t.types.map((u, i) => show(u, i < t.types.length - 1)).join(' ') + ')' :
      t.types.length ? t.name + '[' + t.types.map(show).join(' ') + ']' :
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
  const test = (expect, src) => {
    const actual = showType(run(src).at(-1).type)
    if (actual !== expect) {
      console.error(`${expect} != ${actual}\n${src}`)
      throw new Error(src)
    }
  }

  // primitives
  test("int", "1")
  test("bool", "true")
  test("bool", "false")

  // unary operator
  test("bool", "(! true)")
  test("int", "(- 1)")

  // binary operator
  test("int", "(+ 1 1)")
  test("bool", "(< 1 1)")

  // branch
  test("int", "(iif true 1 2)")
  test("bool", "(iif true true true)")

  // function
  test("(int int)", "(fn a (- a 1))")
  test("(int int int)", "(fn a b (- a b 0))")
  test("int", "((fn 1))")

  // generics
  test("(1 1)", "(fn a a)")
  test("(1 2 1)", "(fn a b a)")
  test("(1 2 2)", "(fn a b b)")
  test("int", "((fn a a) 1)")
  test("bool", "(let f (fn a a)) (f 1) (f true)")

  // property
  test("string", '(catch (throw "a") (fn e (. e message)))')

  // container
  test("vec[1]", "(vec)")
  test("vec[int]", "(vec 1)")
  test("vec[tuple[int bool]]", "(vec (tuple 1 true))")
  test("map[int bool]", "(map 1 true)")

  // combinations
  test("int",                           "(let f (fn x (+ x 1))) (let g (fn x (+ x 2))) (+ (f 1) (g 1))")
  test("((1 2) (2 3) 1 3)",             "(fn f g x (g (f x)))")
  test("((1 2 3) (1 2) 1 3)",           "(fn x y z (x z (y z)))")
  test("(1 (1 bool) (1 1))",            "(fn b x (iif (x b) x (fn x b)))")
  test("(bool bool)",                   "(fn x (iif true x (iif x true false)))")
  test("(bool bool bool)",              "(fn x y (iif x x y))")
  test("(1 1)",                         "(fn n ((fn x (x (fn y y))) (fn f (f n))))")
  test("((1 2) 1 2)",                   "(fn x y (x y))")
  test("((1 2) ((1 2) 1) 2)",           "(fn x y (x (y x)))")
  test("(1 ((1 2 3) 4 2) (1 2 3) 4 3)", "(fn h t f x (f h (t f x)))")
  test("((1 1 2) ((1 1 2) 1) 2)",       "(fn x y (x (y x) (y x)))")
  test("(((1 1) 2) 2)",                 "(let id (fn x x)) (let f (fn y (id (y id))))")
  test("(int)",                         "(let id (fn x x)) (let f (fn (iif (id true) (id 1) (id 2))))")
  test("(int)",                         "(let f (fn x 1)) (let g (fn (+ (f true) (f 4))))")
  test("(bool (1 1))",                  "(let f (fn x x)) (let g (fn y y)) (let h (fn b (iif b (f g) (g f))))")

  // variadic arguments
  test("(int? int)", "(fn a? (+ a 0))")
  test("int", "((fn a? (+ a 0)) 1)")
  test("(int int? int)", "(fn a b? (+ a b 0))")
  test("int", "((fn a b? (+ a b 0)) 1)")
  test("int", "((fn a b? (+ a b 0)) 1 2)")
  test("(1* vec[1])", "(fn a* a)")
  test("vec[int]", "((fn a* a) 1)")
  test("int", "((fn a b* a) 1 2)")
  test("vec[int]", "((fn a b* b) 1 2)")

  // type errors
  reject("(+ 1 true)")
  reject("(iif 1 2 3)")
  reject("(iif true 2 false)")
  reject("((fn e (. e message)) 1)")
}

const testGenerate = param => {
  const test = (expected, exp, define="") => {
    const src = define + "\n((. io put) " + exp + ")"
    const go = param.compile(src, "test.moa")
    const actual = param.runGo(go, src)
    if (expected !== actual) {
      throw new Error(`'${expected}' != '${actual}'\n# src\n${src}\n# go\n${go.def}\n${go.body}\n${go.exp}`)
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
  test("a at test.moa:1:17", "(f)", '(let f (fn (do (throw "a") (return "b"))))')
  test("a", '(catch (f) (fn x "b"))', '(let f (fn "a"))')
  test("c", '(catch (f) (fn x "c"))', '(let f (fn (do (throw "a") (return "b"))))')
  test("a at test.moa:1:17", "(catch (f) (fn x (. x message)))", '(let f (fn (do (throw "a") (return "b"))))')
  test("a at test.moa:1:17", '(iif false "" (catch (f) (fn x (. x message))))', '(let f (fn (do (throw "a") (return "b"))))')
  test("a at test.moa:1:17", '(catch (iif false (f) (f)) (fn x (. x message)))', '(let f (fn (do (throw "a") (return "b"))))')

  // Statement
  test("1", "(iif true 1 2)")
  test("2", "(iif false 1 2)")
  test("1", "((fn (do (if true (return 1)) (return 2))))")
  test("2", "((fn (do (if false (return 1)) (return 2))))")
  test("2", "((fn (do (if false (return 1) (return 2)))))")
  test("9", "a", "(let a 0) (for b 1 6 2 (+= a b))")
  test("6", "a", "(let a 0) (each b (vec 1 2 3) (+= a b))")
  test("6", "a", "(let a 0) (while (< a 6) (+= a 1))")
  test("1", "a", "(let a 0) (for b 0 3 1 (do (+= a 1) break))")
  test("0", "a", "(let a 0) (for b 0 3 1 (do continue (+= a 1)))")

  // Generics
  test("1", "(iif (f true) (f 1) (f 2))", "(let f (fn a a))")
  test("{1}", "(a 1)", "(struct a (t) b t)")
  test("{1 true}", "(a 1 true)", "(struct a (t u) b t c u)")

  // IO
  test("hi", '""', '((. io put) "hi")')
  test("hi", '((. ((. io fetch) "http://localhost:8888") text))', '(async ((. io serve) ":8888" (fn req ((. req respond) 200 (map "content-type" (vec "text/plain")) "hi")))) ((. io sleep) 1.0)')
}

module.exports.test = param => {
  console.clear()
  testSugar(param)
  testInfer(param)
  const fs = require("fs")
  const {execSync} = require("child_process")
  const runtime = fs.readFileSync("runtime.go")
  const cache = fs.existsSync("/tmp/moa_test_cache.json") ? require("/tmp/moa_test_cache.json") : {}
  try {
    param.runGo = (x, src) => {
      const go = `${runtime}\n${x.def}\nfunc main() {
/*
${src.trim()}
*/
  defer func() {
    r := recover()
    if r != nil {
      if e, ok := r.(MoaError); ok {
        fmt.Print(e.Error())
      }
    }
  }()
  ${x.body}
}\n`
      const key = x.def + x.body
      if (key in cache) {
        return cache[key]
      }
      fs.writeFileSync("/tmp/moa_test_exp.go", go)
      return cache[key] = execSync("go run /tmp/moa_test_exp.go").toString()
    }
    testGenerate(param)
  } catch (e) {
    console.error(e.stack)
    return 1
  } finally {
    fs.writeFileSync("/tmp/moa_test_cache.json", JSON.stringify(cache, null, 2))
  }
}
