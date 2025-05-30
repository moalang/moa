"use strict"
import vm from "vm"
import {parse} from "./parse.js"
import {infer} from "./infer.js"

const range = n => [...new Array(n)].map((_, i) => i)
const log = x => { console.dir(x, {depth: null}); return x }

const generate = root => {
  const isOp = code => /^[+\-*/%|&<>=!\^~]+$/.test(code) && code !== "=>"
  const genid = x => x.code
  const genstruct = a => "(" + a + ") => ({" + a + "})"
  const genfor = (a,b,c,d,e) => "for (let " + a + "=" + b + "; " + a + "<" + c + ";" + a + "+=" + d + ") " + e
  const gencall = (x, xs) =>
    Array.isArray(x)                               ? gen(x) + "(" + xs.map(gen) + ")" :
    x.code === "." && xs[1].code.match(/^[0-9]/)   ? gen(xs[0]) + "[" + gen(xs[1]) + "]" :
    x.code === "."                                 ? gen(xs[0]) + x.code + gen(xs[1]) :
    isOp(x.code) && xs.length === 2                ? gen(xs[0]) + x.code + gen(xs[1]) :
    isOp(x.code) && xs.length === 1                ? x.code + gen(xs[0]) :
    x.code === "new"                               ? "({" + range(xs.length/2).map(i => xs[i*2].code + ":" + gen(xs[i*2+1])) + "})" :
    x.code === "fn"                                ? "((" + xs.slice(0, -1).map(genid) + ") => " + gen(xs.at(-1)) + ")" :
    x.code === "let"                               ? "const " + xs[0].code + " = " + gen(xs[1]) :
    x.code === "var"                               ? "let "   + xs[0].code + " = " + gen(xs[1]) :
    x.code === "def"                               ? "const " + xs[0].code + " = " + "(" + xs.slice(1, -1).map(genid) + ") => " + gen(xs.at(-1)) :
    x.code === "struct"                            ? "const " + xs[0].code + " = " + genstruct(range(xs.length/2-1).map(i => xs[i*2+2].code)) :
    x.code === "do"                                ? "{" + xs.map(gen).join(";\n") + "}" :
    x.code === "throw"                             ? "moa_throw(" + gen(xs[0]) + ", " + JSON.stringify(x.filename + ":" + x.lineno + ":" + x.column) + ")" :
    x.code === "catch"                             ? "moa_catch(() => " + gen(xs[0]) + ", " + gen(xs[1]) + ")" :
    x.code === "iif"                               ? "0 || (" + gen(xs[0]) + " ? " + gen(xs[1]) + ":" + gen(xs[2]) + ")" :
    x.code === "if"                                ? "if (" + gen(xs[0]) + ") " + gen(xs[1]) + (xs[2] ? "; else " + gen(xs[2]) : "") :
    x.code === "for"                               ? genfor(...xs.map(gen)) :
    x.code === "each" && xs[2].type.name === "vec" ? "let " + xs[0].code + " = 0; for (const " + xs[1].code + " of " + gen(xs[2]) + ") { " + gen(xs[3]) + "; " + xs[0].code + "++ }" :
    x.code === "each" && xs[2].type.name === "map" ? "for (const [" + xs[0].code + ", " + xs[1].code + "] of [..." + gen(xs[2]) + "]) " + gen(xs[3]) :
    x.code === "while"                             ? "while (" + gen(xs[0]) + ") " + gen(xs[1]) :
    x.code + "(" + xs.map(gen) + ")"
  const gen = x => Array.isArray(x) ? gencall(x[0], x.slice(1)) : x.code
  return root.map(gen).join("\n")
}

const runtime = (() => {
const moa_catch = (f, g) => {
  try {
    return f()
  } catch (e) {
    return g(e)
  }
}
const tuple = (...a) => (a.__tuple=true, a)
const vec = (...a) => a
const map = (...a) => new Map([...new Array(a.length/2)].map((_, i) => [a[i*2], a[i*2+1]]))
const set = (...a) => new Set(a)
class MoaError extends Error {
  constructor(message, location) {
    super(message)
    this.location = location
  }
}
const moa_throw = (message, location) => {
  throw new MoaError(message, location)
}
}).toString().slice(8, -2)

const evaluate = (node, context={}) => {
  const js = generate(node)
  context.console = console
  try {
    return new vm.Script(runtime + ";" + js, {timeout: 100}).runInNewContext(context)
  } catch (e) {
    return e
  }
}

export {evaluate, generate}
