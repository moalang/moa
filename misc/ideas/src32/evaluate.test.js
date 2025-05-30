"use strict"
import {describe, it, expect} from "bun:test";
import {infer} from "./infer.js"
import {parseWithoutSugar} from "./parse.js"
import {evaluate} from "./evaluate.js"

describe("evaluate", () => {
  const show = x =>
    x === undefined ? "(undefined)" :
    x === null ? "(null)" :
    x.constructor.name === "MoaError" ? x.message + " at " + x.location :
    x.constructor.name === "Set"      ? "set(" + [...x].map(show).join(" ") + ")" :
    x.constructor.name === "Map"      ? "map(" + [...x].flatMap(a => a).map(show).join(" ") + ")" :
    x.constructor.name === "Array"    ? (x.__tuple ? "tuple(" + x.map(show).join(" ") + ")" : "[" + x.map(show).join(" ") + "]") :
    x.constructor.name === "Object"   ? "new(" + Object.entries(x).flatMap(a => [a[0], show(a[1])]).join(" ") + ")" :
    x.toString()
  const run = (exp, define="") => show(evaluate(infer(parseWithoutSugar((define + "\n" + exp).trim(), "test.moa"))))

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

  it("method", () => {
    expect(run("(. (tuple 1) 0)")).toBe("1")
    expect(run("(. (tuple 1 2) 1)")).toBe("2")
    expect(run("(. (new a 1) a)")).toBe("1")
    expect(run("(. (new a 1 b 2) b)")).toBe("2")
    expect(run("((fn a (. a 0)) (tuple 1))")).toBe("1")
    expect(run("((fn a (. a b)) (new b 1))")).toBe("1")
  })

  it("lambda", () => {
    expect(run("((fn true))")).toBe("true")
    expect(run("((fn 1))")).toBe("1")
    expect(run("((fn 1.2))")).toBe("1.2")
    expect(run('((fn "hi"))')).toBe("hi")
    expect(run("((fn a a) 1)")).toBe("1")
    expect(run("((fn a b (+ a b)) 1 2)")).toBe("3")
  })

  it("define", () => {
    expect(run("a", "(let a 1)")).toBe("1")
    expect(run("(f)", "(let f (fn 1))")).toBe("1")
    expect(run("(f)", "(let f (fn (do (return 1))))")).toBe("1")
    expect(run("(f 1)", "(let f (fn a a))")).toBe("1")
    expect(run("(f 1 2)", "(let f (fn a b (+ a b)))")).toBe("3")
    expect(run("(a)", "(def a 1)")).toBe("1")
    expect(run("(a 2)", "(def a b b)")).toBe("2")
    expect(run("(a 1 2)", "(def a b c (+ b c))")).toBe("3")
    expect(run("(a 1)", "(struct a () b int)")).toBe("new(b 1)")
    expect(run("(a 1 true)", "(struct a () b int c bool)")).toBe("new(b 1 c true)")
    expect(run("(. (a 1) b)", "(struct a () b int)")).toBe("1")
    expect(run("(. (a 1 false) c)", "(struct a () b int c bool)")).toBe("false")
    expect(run("(vec (a 1) (new b 2))", "(struct a () b int)")).toBe("[new(b 1) new(b 2)]")
  })

  it("statement", () => {
    expect(run("(iif true 1 2)")).toBe("1")
    expect(run("(iif false 1 2)")).toBe("2")
    expect(run("((fn (do (if true (return 1)) (return 2))))")).toBe("1")
    expect(run("((fn (do (if false (return 1)) (return 2))))")).toBe("2")
    expect(run("((fn (do (if false (return 1) (return 2)))))")).toBe("2")
    expect(run("a", "(var a 0) (for b 1 6 2 (+= a b))")).toBe("9")
    expect(run("a", "(var a 0) (each b _ (vec 1 2 3) (+= a b))")).toBe("3")
    expect(run("a", "(var a 0) (each _ b (vec 1 2 3) (+= a b))")).toBe("6")
    expect(run("a", "(var a 0) (each b c (map 1 3 2 3) (+= a (+ b c)))")).toBe("9")
    expect(run("a", "(var a 0) (while (< a 6) (+= a 1))")).toBe("6")
    expect(run("a", "(var a 0) (for b 0 3 1 (do (+= a 1) break))")).toBe("1")
    expect(run("a", "(var a 0) (for b 0 3 1 (do continue (+= a 1)))")).toBe("0")
  })

  it("throw / catch", () => {
    expect(run("(f)", '(let f (fn (do (throw "a") (return "b"))))')).toBe("a at test.moa:1:1")
    expect(run('(catch (f) (fn x "b"))', '(let f (fn "a"))')).toBe("a")
    expect(run('(catch (f) (fn x "c"))', '(let f (fn (do (throw "a") (return "b"))))')).toBe("c")
    expect(run('(catch (f) (fn x (. x message)))', '(let f (fn (do (throw "a") (return "b"))))')).toBe("a")
    expect(run('(iif false "" (catch (f) (fn x (. x message))))', '(let f (fn (do (throw "a") (return "b"))))')).toBe("a")
    expect(run('(catch (iif false (f) (f)) (fn x (. x message)))', '(let f (fn (do (throw "a") (return "b"))))')).toBe("a")
  })

  it("generic", () => {
    expect(run("(iif (f true) (f 1) (f 2))", "(let f (fn a a))")).toBe("1")
    expect(run("(iif (f true) (f 1) (f 2))", "(def f a a)")).toBe("1")
    expect(run("(a 1)", "(struct a (t) b t)")).toBe("new(b 1)")
    expect(run("(a 1 true)", "(struct a (t u) b t c u)")).toBe("new(b 1 c true)")
  })
})
