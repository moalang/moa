"use strict"
import vm from "vm"
import {parse} from "./parse.js"
import {infer} from "./infer.js"

const range = n => [...new Array(n)].map((_, i) => i)

const generate = root => {
  const isOp = code => /^[+\-*/%|&<>=!\^~]+$/.test(code) && code !== "=>"
  const genid = x => x.code
  const genstruct = a => "(" + a + ") => ({" + a + "})"
  const gencall = (x, xs) =>
    Array.isArray(x)                             ? gen(x) + "(" + xs.map(gen) + ")" :
    x.code === "." && xs[1].code.match(/^[0-9]/) ? gen(xs[0]) + "[" + gen(xs[1]) + "]" :
    x.code === "."                               ? gen(xs[0]) + x.code + gen(xs[1]) :
    isOp(x.code) && xs.length === 2              ? gen(xs[0]) + x.code + gen(xs[1]) :
    isOp(x.code) && xs.length === 1              ? x.code + gen(xs[0]) :
    x.code === "new"                             ? "({" + range(xs.length/2).map(i => xs[i*2].code + ":" + gen(xs[i*2+1])) + "})" :
    x.code === "fn"                              ? "((" + xs.slice(0, -1).map(genid) + ") => " + gen(xs.at(-1)) + ")" :
    x.code === "let"                             ? "const " + xs[0].code + " = " + gen(xs[1]) :
    x.code === "var"                             ? "let "   + xs[0].code + " = " + gen(xs[1]) :
    x.code === "def"                             ? "const " + xs[0].code + " = " + "(" + xs.slice(1, -1).map(genid) + ") => " + gen(xs.at(-1)) :
    x.code === "struct"                          ? "const " + xs[0].code + " = " + genstruct(range(xs.length/2-1).map(i => xs[i*2+2].code)) :
    x.code === "do"                              ? "{" + xs.map(gen).join(";\n") + "}" :
    x.code + "(" + xs.map(gen) + ")"
  const gen = x => Array.isArray(x) ? gencall(x[0], x.slice(1)) : x.code
  return root.map(gen).join("\n")
}

const evaluate = (node, context={}) => {
  const js = generate(node)
  context.tuple ||= (...a) => (a.__tuple=true, a)
  context.vec ||= (...a) => a
  context.map ||= (...a) => new Map(range(a.length/2).map(i => [a[i*2], a[i*2+1]]))
  context.set ||= (...a) => new Set(a)
  try {
    return new vm.Script(js).runInNewContext(context)
  } catch (e) {
    return e
  }
}

export {evaluate, generate}
