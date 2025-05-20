"use strict"
import { describe, it, expect } from "bun:test";
import { sugar, parse, infer, generate, TypeError } from "./moa.js"

const log = x => { console.dir(x, {depth: null}); return x }

describe("desugar", () => {
  const show = o => Array.isArray(o) ? `(${o.map(show).join(" ")})` : o.code
  const run = src => show(sugar(src, "test.moa")[0])

  it("basic", () => {
    expect(run("a")).toBe("a")
    expect(run("a b")).toBe("(a b)")
    expect(run("a()")).toBe("(a)")
    expect(run("a(b)")).toBe("(a b)")
    expect(run("!true")).toBe("(! true)")
    expect(run("1 + 2")).toBe("(+ 1 2)")
    expect(run("1 + 2 + 3")).toBe("(+ 1 (+ 2 3))")
    expect(run("a.b")).toBe("(. a b)")
    expect(run("a.b c")).toBe("((. a b) c)")
    expect(run("a[b]")).toBe("((. a at) b)")
    expect(run("a[+ b c]")).toBe("((. a at) (+ b c))")
    expect(run("a => a")).toBe("(fn a a)")
    expect(run("a,b => a + b")).toBe("(fn a b (+ a b))")
    expect(run("a =>\n  a\n  b c")).toBe("(fn a (do a (b c)))")
    expect(run("a: b")).toBe("(a b)")
    expect(run("a: b c")).toBe("(a (b c))")
    expect(run("a b: c")).toBe("(a b c)")
    expect(run("a :\n  b\n  c d")).toBe("(a (do b (c d)))")
  })

  it("combination", () => {
    expect(run("a.b c => d")).toBe("((. a b) (fn c d))")
    expect(run("a.b c d => e.f")).toBe("((. a b) c (fn d (. e f)))")
    expect(run("a.b + c")).toBe("(+ (. a b) c)")
    expect(run("a + b.c")).toBe("(+ a (. b c))")
    expect(run("a:\n  b:\n    c")).toBe("(a (b c))")
    expect(run("a:\n  b\n  c:\n    d")).toBe("(a (do b (c d)))")
    expect(run("a:\n  b\n  c:\n    d\n    e")).toBe("(a (do b (c (do d e))))")
    expect(run("a =>\n  b =>\n    c")).toBe("(fn a (fn b c))")
    expect(run("a =>\n  b\n  c =>\n    d")).toBe("(fn a (do b (fn c d)))")
    expect(run("a =>\n  b\n  c =>\n    d\n    e")).toBe("(fn a (do b (fn c (do d e))))")
  })

  it("variable arguments", () => {
    expect(run("+ 1 2 3")).toBe("(+ 1 (+ 2 3))")
    expect(run("(+ 1 2 3)")).toBe("(+ 1 (+ 2 3))")
    expect(run("(a (+ 1 2 3))")).toBe("(a (+ 1 (+ 2 3)))")
    expect(run("a[+ b c d]")).toBe("((. a at) (+ b (+ c d)))")
    expect(run("iif a b c")).toBe("(iif a b c)")
    expect(run("iif a b c d e")).toBe("(iif a b (iif c d e))")
    expect(run("if a b c d")).toBe("(if a (if b c d))")
    expect(run("if a b c d e")).toBe("(if a b (if c d e))")
    expect(run("if a b c d e f")).toBe("(if a b (if c (if d e f)))")
    expect(run("if a b c d e f g")).toBe("(if a b (if c d (if e f g)))")
    expect(run("for 3 a")).toBe("(for _ 0 3 1 a)")
    expect(run("for i 3 a")).toBe("(for i 0 3 1 a)")
    expect(run("for i 1 3 a")).toBe("(for i 1 3 1 a)")
    expect(run("for i 1 3 2 a")).toBe("(for i 1 3 2 a)")
    expect(run("each a b c")).toBe("(each _ a b c)")
    expect(run("each a b c d")).toBe("(each a b c d)")
  })
})

describe("infer", () => {
  const showType = type => {
    const show = (t, mark) => t.instance ? show(t.instance, mark) :
      mark && t.mark ? (t.mark.includes("*") ? show(t.types[0]) + "*" : t.name + t.mark) :
      t.struct ? t.struct :
      t.name === "fn" ? "(" + t.types.map((u, i) => show(u, i < t.types.length - 1)).join(" ") + ")" :
      t.types.length ? t.name + "[" + t.types.map(show).join(" ") + "]" :
      t.name + (t.props ? Object.keys(t.props).map(p => "." + p + "(" + show(t.props[p]) + ")").join("") : "")
    const s = show(type)
    const o = {}
    const r = s.replace(/\d+/g, t => o[t] ||= Object.keys(o).length + 1)
    return r
  }
  const reject = src => {
    try {
      infer(parse(src))
      throw new Error(src)
    } catch (e) {
      if (!(e instanceof TypeError)) {
        throw e
      }
    }
  }

  const run = src => showType(infer(parse(src)).at(-1).type)

  it("primitives", () => {
    expect(run("1")).toBe("int")
    expect(run("true")).toBe("bool")
    expect(run("false")).toBe("bool")
  })

  it("unary operator", () => {
    expect(run("(! true)")).toBe("bool")
    expect(run("(- 1)")).toBe("int")
  })

  it("binary operator", () => {
    expect(run("(+ 1 1)")).toBe("int")
    expect(run("(< 1 1)")).toBe("bool")
  })

  it("branch", () => {
    expect(run("(iif true 1 2)")).toBe("int")
    expect(run("(iif true true true)")).toBe("bool")
  })

  it("function", () => {
    expect(run("(fn a (- a 1))")).toBe("(int int)")
    expect(run("(fn a b (- a b 0))")).toBe("(int int int)")
    expect(run("((fn 1))")).toBe("int")
  })

  it("do", () => {
    expect(run("(do 1)")).toBe("int")
    expect(run("(do 1 true)")).toBe("bool")
    expect(run("(do (if false (return 1)) ((. io put) 2))")).toBe("int")
  })

  it("generics", () => {
    expect(run("(fn a a)")).toBe("(1 1)")
    expect(run("(fn a b a)")).toBe("(1 2 1)")
    expect(run("(fn a b b)")).toBe("(1 2 2)")
    expect(run("((fn a a) 1)")).toBe("int")
    expect(run("(let f (fn a a)) (f 1) (f true)")).toBe("bool")
    expect(run("(def f a a) (f 1) (f true)")).toBe("bool")
  })

  it("property", () => {
    expect(run("(let a (fn b (. b text)))")).toBe("(1.text(2) 2)")
    expect(run('(catch (throw "a") (fn e (. e message)))')).toBe("string")
    expect(run("(vec (new a 2))")).toBe("vec[new__a[int]]")
    expect(run("(struct a () b int) (vec (a 1))")).toBe("vec[a]")
    expect(run("(struct a () b int) (vec (a 1) (new b 2))")).toBe("vec[a]")
    expect(run("(vec (new a 2))")).toBe("vec[new__a[int]]")
    expect(run("(struct a () b int) (vec (a 1))")).toBe("vec[a]")
    expect(run("(struct a () b int) (vec (a 1) (new b 2))")).toBe("vec[a]")
    expect(run("(struct a () b int) (vec (new b 1) (a 2))")).toBe("vec[a]")
    expect(run("(struct a () b int c int) (vec (new b 1) (a 2 3))")).toBe("vec[a]")
  })

  it("container", () => {
    expect(run("(vec)")).toBe("vec[1]")
    expect(run("(vec 1)")).toBe("vec[int]")
    expect(run("(vec (tuple 1 true))")).toBe("vec[tuple[int bool]]")
    expect(run("(map 1 true)")).toBe("map[int bool]")
  })

  it("combinations", () => {
    expect(run("(let f (fn x (+ x 1))) (let g (fn x (+ x 2))) (+ (f 1) (g 1))")).toBe("int")
    expect(run("(fn f g x (g (f x)))")).toBe("((1 2) (2 3) 1 3)")
    expect(run("(fn x y z (x z (y z)))")).toBe("((1 2 3) (1 2) 1 3)")
    expect(run("(fn b x (iif (x b) x (fn x b)))")).toBe("(1 (1 bool) (1 1))")
    expect(run("(fn x (iif true x (iif x true false)))")).toBe("(bool bool)")
    expect(run("(fn x y (iif x x y))")).toBe("(bool bool bool)")
    expect(run("(fn n ((fn x (x (fn y y))) (fn f (f n))))")).toBe("(1 1)")
    expect(run("(fn x y (x y))")).toBe("((1 2) 1 2)")
    expect(run("(fn x y (x (y x)))")).toBe("((1 2) ((1 2) 1) 2)")
    expect(run("(fn h t f x (f h (t f x)))")).toBe("(1 ((1 2 3) 4 2) (1 2 3) 4 3)")
    expect(run("(fn x y (x (y x) (y x)))")).toBe("((1 1 2) ((1 1 2) 1) 2)")
    expect(run("(let id (fn x x)) (let f (fn y (id (y id))))")).toBe("(((1 1) 2) 2)")
    expect(run("(let id (fn x x)) (let f (fn (iif (id true) (id 1) (id 2))))")).toBe("(int)")
    expect(run("(let f (fn x 1)) (let g (fn (+ (f true) (f 4))))")).toBe("(int)")
    expect(run("(let f (fn x x)) (let g (fn y y)) (let h (fn b (iif b (f g) (g f))))")).toBe("(bool (1 1))")
  })

  it("variadic arguments", () => {
    expect(run("(fn a? (+ a 0))")).toBe("(int? int)")
    expect(run("((fn a? (+ a 0)) 1)")).toBe("int")
    expect(run("(fn a b? (+ a b 0))")).toBe("(int int? int)")
    expect(run("((fn a b? (+ a b 0)) 1)")).toBe("int")
    expect(run("((fn a b? (+ a b 0)) 1 2)")).toBe("int")
    expect(run("(fn a* a)")).toBe("(1* vec[1])")
    expect(run("((fn a* a) 1)")).toBe("vec[int]")
    expect(run("((fn a b* a) 1 2)")).toBe("int")
    expect(run("((fn a b* b) 1 2)")).toBe("vec[int]")
  })

  it("io", () => {
    expect(run('((. ((. io fetch) "") text))')).toBe("string")
    expect(run('((. ((. io fetch) "") header) "")')).toBe("string")
    expect(run('((. ((. io fetch) "") cookie) "")')).toBe("string")
  })

  it("reject invalid types", () => {
    reject("(+ 1 true)")
    reject("(iif 1 2 3)")
    reject("(iif true 2 false)")
    reject("((fn e (. e message)) 1)")
    reject("(struct a () b int) (vec (a 1) (new c 2))")
  })
})

const testGenerate = param => {
  const test = (expected, exp, define="") => {
    const src = define + "\n((. io put) " + exp + ")"
    const go = param.generate(param.infer(param.parse(src, "test.moa")))
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
  test("[]", "(vec)")
  test("[1]", "(vec 1)")
  test("[1 2]", "(vec 1 2)")
  test("[true]", "(vec true)")
  test("map[]", "(map)")
  test("map[1:true]", "(map 1 true)")
  test("map[1:true 2:false]", "(map 1 true 2 false)")
  test("map[]", "(set)")
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
  test("1", "(a)", "(def a 1)")
  test("2", "(a 2)", "(def a b b)")
  test("3", "(a 1 2)", "(def a b c (+ b c))")
  test("{1}", "(a 1)", "(struct a () b int)")
  test("{1 true}", "(a 1 true)", "(struct a () b int c bool)")
  test("1", "(. (a 1) b)", "(struct a () b int)")
  test("false", "(. (a 1 false) c)", "(struct a () b int c bool)")
  test("[{1} {2}]", "(vec (a 1) (new b 2))", "(struct a () b int)")

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
  test("3", "a", "(let a 0) (each b _ (vec 1 2 3) (+= a b))")
  test("6", "a", "(let a 0) (each _ b (vec 1 2 3) (+= a b))")
  test("9", "a", "(let a 0) (each b c (map 1 3 2 3) (+= a (+ b c)))")
  test("6", "a", "(let a 0) (while (< a 6) (+= a 1))")
  test("1", "a", "(let a 0) (for b 0 3 1 (do (+= a 1) break))")
  test("0", "a", "(let a 0) (for b 0 3 1 (do continue (+= a 1)))")

  // Generics
  test("1", "(iif (f true) (f 1) (f 2))", "(let f (fn a a))")
  test("1", "(iif (f true) (f 1) (f 2))", "(def f a a)")
  test("{1}", "(a 1)", "(struct a (t) b t)")
  test("{1 true}", "(a 1 true)", "(struct a (t u) b t c u)")

  // IO
  test("hi", '""', '((. io put) "hi")')
  test("hi", '((. ((. io fetch) "http://localhost:8888") text))', '(async ((. io serve) ":8888" (fn req (new text "hi")))) ((. io sleep) 1.0)')
  test("hi", '((. ((. io fetch) "http://localhost:8888" (new method "POST")) text))', '(async ((. io serve) ":8888" (fn req (new text "hi")))) ((. io sleep) 1.0)')
}
