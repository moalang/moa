"use strict"
import {newtoken, parseWithoutSugar} from "./parse.js"
import {infer, TypeError} from "./infer.js"

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

describe("infer", () => {
  const reject = src => {
    try {
      infer(parseWithoutSugar(src))
      throw new Error(src)
    } catch (e) {
      if (!(e instanceof TypeError)) {
        throw e
      }
    }
  }

  const run = src => showType(infer(parseWithoutSugar(src)).at(-1).type)

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
