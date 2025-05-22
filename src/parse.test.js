"use strict"
import {describe, it, expect} from "bun:test";
import {parse} from './parse.js'

describe("parse", () => {
  const show = o => Array.isArray(o) ? `(${o.map(show).join(" ")})` : o.code
  const run = src => show(parse(src, "test.moa")[0])

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
