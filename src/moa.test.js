"use strict"
import { describe, it, expect } from "bun:test";
import { sugar, parse, infer, evaluate, TypeError } from "./moa.js"

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
    expect(run("~1")).toBe("(~ 1)")
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

describe("evaluate", () => {
  const show = x =>
    x === undefined ? "(undefined)" :
    x instanceof Set ? "set(" + [...x].map(show).join(" ") + ")" :
    x instanceof Map ? "map(" + [...x].flatMap(a => a).map(show).join(" ") + ")" :
    Array.isArray(x) ? (x.__tuple ? "tuple(" + x.map(show).join(" ") + ")" : "[" + x.map(show).join(" ") + "]") :
      typeof x === "object" ? "new(" + Object.entries(x).flatMap(a => [a[0], show(a[1])]).join(" ") + ")" :
    x.toString()
  const run = (exp, define="") => show(evaluate((define + "\n" + exp).trim()))

  it("value", () => {
    expect(run("true")).toBe("true")
    expect(run("1")).toBe("1")
    expect(run("1.2")).toBe("1.2")
    expect(run('"hi"')).toBe("hi")
    expect(run("(tuple 1)")).toBe("tuple(1)")
    expect(run("(tuple 1 2)")).toBe("tuple(1 2)")
    expect(run("(new a 1)")).toBe("new(a 1)")
    expect(run("(new a 1 b 2)")).toBe("new(a 1 b 2)")
    expect(run("(vec)")).toBe("[]")
    expect(run("(vec 1)")).toBe("[1]")
    expect(run("(vec 1 2)")).toBe("[1 2]")
    expect(run("(vec true)")).toBe("[true]")
    expect(run("(map)")).toBe("map()")
    expect(run("(map 1 true)")).toBe("map(1 true)")
    expect(run("(map 1 true 2 false)")).toBe("map(1 true 2 false)")
    expect(run("(set)")).toBe("set()")
    expect(run("(set 1)")).toBe("set(1)")
    expect(run("(set 1 2)")).toBe("set(1 2)")
  })

  it("operator", () => {
    expect(run("(! true)")).toBe("false")
    expect(run("(- 1)")).toBe("-1")
    expect(run("(~ 1)")).toBe("-2")
    expect(run("(+ 1 2)")).toBe("3")
    expect(run("(- 3 2)")).toBe("1")
    expect(run("(* 2 3)")).toBe("6")
    expect(run("(/ 4 2)")).toBe("2")
    expect(run("(% 3 2)")).toBe("1")
    expect(run("(^ 1 2)")).toBe("3")
    expect(run("(== 1 1)")).toBe("true")
    expect(run("(!= 1 1)")).toBe("false")
    expect(run("(< 1 1)")).toBe("false")
    expect(run("(> 1 1)")).toBe("false")
    expect(run("(<= 1 1)")).toBe("true")
    expect(run("(>= 1 1)")).toBe("true")
  })

  it.skip("method", () => {
    expect(run("(. (tuple 1) 0)")).toBe("1")
    expect(run("(. (tuple 1 2) 1)")).toBe("2")
    expect(run("(. (new a 1) a)")).toBe("1")
    expect(run("(. (new a 1 b 2) b)")).toBe("2")
    expect(run("((fn a (. a 0)) (tuple 1))")).toBe("1")
    expect(run("((fn a (. a b)) (new b 1))")).toBe("1")
  })

  it.skip("lambda", () => {
    expect(run("((fn true))")).toBe("true")
    expect(run("((fn 1))")).toBe("1")
    expect(run("((fn 1.2))")).toBe("1.2")
    expect(run('((fn "hi"))')).toBe("hi")
    expect(run("((fn a a) 1)")).toBe("1")
    expect(run("((fn a b (+ a b)) 1 2)")).toBe("3")
  })

  it.skip("define", () => {
    expect(run("a", "(let a 1)")).toBe("1")
    expect(run("(f)", "(let f (fn 1))")).toBe("1")
    expect(run("(f)", "(let f (fn (do (return 1))))")).toBe("1")
    expect(run("(f 1)", "(let f (fn a a)")).toBe("1")
    expect(run("(f 1 2)", "(let f (fn a b (+ a b)))")).toBe("3")
    expect(run("(a)", "(def a 1)")).toBe("1")
    expect(run("(a 2)", "(def a b b)")).toBe("2")
    expect(run("(a 1 2)", "(def a b c (+ b c))")).toBe("3")
    expect(run("(a 1)", "(struct a () b int)")).toBe("{1}")
    expect(run("(a 1 true)", "(struct a () b int c bool)")).toBe("{1 true}")
    expect(run("(. (a 1) b)", "(struct a () b int)")).toBe("1")
    expect(run("(. (a 1 false) c)", "(struct a () b int c bool)")).toBe("false")
    expect(run("(vec (a 1) (new b 2))", "(struct a () b int)")).toBe("[{1} {2}]")
  })

  it.skip("throw / catch", () => {
    expect(run("(f)", '(let f (fn (do (throw "a") (return "b"))))')).toBe("a at test.moa:1:17")
    expect(run('(catch (f) (fn x "b"))', '(let f (fn "a"))')).toBe("a")
    expect(run('(catch (f) (fn x "c"))', '(let f (fn (do (throw "a") (return "b"))))')).toBe("c")
    expect(run('(catch (f) (fn x (. x message)))', '(let f (fn (do (throw "a") (return "b"))))')).toBe("a at test.moa:1:17")
    expect(run('(iif false "" (catch (f) (fn x (. x message))))', '(let f (fn (do (throw "a") (return "b"))))')).toBe("a at test.moa:1:17")
    expect(run('(catch (iif false (f) (f)) (fn x (. x message)))', '(let f (fn (do (throw "a") (return "b"))))')).toBe("a at test.moa:1:17")
  })

  it.skip("statement", () => {
    expect(run("(iif true 1 2)")).toBe("1")
    expect(run("(iif false 1 2)")).toBe("2")
    expect(run("((fn (do (if true (return 1)) (return 2))))")).toBe("1")
    expect(run("((fn (do (if false (return 1)) (return 2))))")).toBe("2")
    expect(run("((fn (do (if false (return 1) (return 2)))))")).toBe("2")
    expect(run("a", "(let a 0) (for b 1 6 2 (+= a b))")).toBe("9")
    expect(run("a", "(let a 0) (each b _ (vec 1 2 3) (+= a b))")).toBe("3")
    expect(run("a", "(let a 0) (each _ b (vec 1 2 3) (+= a b))")).toBe("6")
    expect(run("a", "(let a 0) (each b c (map 1 3 2 3) (+= a (+ b c)))")).toBe("9")
    expect(run("a", "(let a 0) (while (< a 6) (+= a 1))")).toBe("6")
    expect(run("a", "(let a 0) (for b 0 3 1 (do (+= a 1) break))")).toBe("1")
    expect(run("a", "(let a 0) (for b 0 3 1 (do continue (+= a 1)))")).toBe("0")
  })

  it.skip("generic", () => {
    expect(run("(iif (f true) (f 1) (f 2))", "(let f (fn a a))")).toBe("1")
    expect(run("(iif (f true) (f 1) (f 2))", "(def f a a)")).toBe("1")
    expect(run("(a 1)", "(struct a (t) b t)")).toBe("{1}")
    expect(run("(a 1 true)", "(struct a (t u) b t c u)")).toBe("{1 true}")
  })

  it.skip("io", () => {
    expect(run('""', '((. io put) "hi")')).toBe("hi")
    expect(run('((. ((. io fetch) "http://localhost:8888") text))', '(async ((. io serve) ":8888" (fn req (new text "hi")))) ((. io sleep) 1.0)')).toBe("hi")
    expect(run('((. ((. io fetch) "http://localhost:8888" (new method "POST")) text))', '(async ((. io serve) ":8888" (fn req (new text "hi")))) ((. io sleep) 1.0)')).toBe("hi")
  })
})
