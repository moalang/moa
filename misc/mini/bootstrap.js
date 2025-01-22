"use strict"
const fs = require("node:fs")
const child_process = require("node:child_process")

function assert(cond, v) {
  if (!cond) {
    throw new Error(JSON.stringify(v))
  }
}

function parse(moa) {
  const tokens = moa.split(/([ \n]+|[()\[\]{}]|[+\-*/%!=^|&?]+|"[^"]*"|[ \n]+)/).filter(s => s.trim().length).map(code => ({code}))
  let pos = 0
  function until(code) {
    const a = []
    while (pos < tokens.length && tokens[pos].code !== code) {
      a.push(parseUnit(tokens[pos++]))
    }
    if (pos < tokens.length && tokens[pos].code === code) {
      pos++
    }
    return a
  }
  function parseUnit(token) {
    return token.code === "(" ? until(")") : token
  }
  return until()
}

function infer(nodes) {
  let typeVariableSequence = 0
  const newType = (name,generics=[],props={}) => ({name,generics,props})
  const newVar = () => ({name: (typeVariableSequence++).toString(), isvar: true})
  const toVariadic = t => ({...t, variadic: true})
  const tvoid = newType("tvoid")
  const tany = ({name: "any", isvar: true})
  const tint = newType("int")
  const tstring = newType("string")
  function prune(t) {
    return t.instance ? (t.instance = prune(t.instance)) : t
  }
  function unify(a, b) {
    a = prune(a)
    b = prune(b)
    if (a.isvar) {
      a.instance = b
    } else if (b.isvar) {
      unify(b, a)
    } else {
      assert(a.name === b.name, {a, b})
      assert(a.generics.length === b.generics.length, {a, b})
      a.generics.map((ag, i) => unify(ag, b.generics[i]))
    }
  }
  function call(f, args) {
    assert(Array.isArray(f), f)
    assert((f.length >= 1 && f.at(-2).variadic) || f.length === args.length - 1, f)
    args.map((a, i) => unify(f[i], a))
    return f.at(-1)
  }
  function inferWith(node, tenv) {
    function lookup(node) {
      assert(node.code in tenv, node)
      return tenv[node.code]
    }
    function inf(node) {
      return node.type = infImpl(node)
    }
    function infImpl(node) {
      if (Array.isArray(node)) {
        const head = node[0]
        const tail = node.slice(1)
        if (Array.isArray(head)) {
          return call(inf(head), tail.map(inf))
        } else if (head.code === "def") {
          const name = tail[0]
          const args = tail.slice(1, -1)
          const vars = args.map(newVar)
          const tinner = {...tenv, ...Object.fromEntries(args.map((a,i) => [a[i].code, vargs[i]]))}
          const body = tail.at(-1).map(node => inferWith(node, tinner)).at(-1)
          return vars.concat([body])
        } else if (head.code === ".") {
          const prop = inf(tail[0]).props[tail[1].code]
          assert(prop, node)
          return prop
        } else {
          assert(node)
        }
      } else if(node.code.startsWith('"')) {
        return tstring
      } else if("0123456789".includes(node.code[0])) {
        return tint
      } else {
        return lookup(node)
      }
    }
    return inf(node)
  }
  const troot = {
    io: newType("io", [], {
      puts: [toVariadic(tany), tvoid]
    })
  }
  nodes.map(node => inferWith(node, troot))
}

function showType(o) {
  function show(o) {
    return Array.isArray(o) ? `${o.map(show).join(":")}` : `${o.variadic ? "..." : ""}${o.name}`
  }
  const m = new Map()
  return show(o).replace(/[0-9]+/g, n => m.get(n) || m.set(n, m.size+1).get(n))
}

function showNode(o) {
  return (Array.isArray(o) ? `(${o.map(showNode).join(" ")})` : o.code || JSON.stringify(o)) + (o.type ? "@" + showType(o.type) : "")
}

function compile(moa) {
  function gen(node) {
    if (Array.isArray(node)) {
      const head = node[0]
      const tail = node.slice(1)
      if (Array.isArray(head)) {
        return `${gen(head)}(${tail.map(gen)})`
      } else if (head.code === "def") {
        const args = tail.slice(1, -1)
        const body = tail.at(-1).map(gen)
        return `func ${tail[0].code}(${args}) {${body}}`
      } else if ("+-*/%!=^|&?<>.".includes(head.code[0])) {
        return gen(tail[0]) + head.code + gen(tail[1])
      } else {
        assert(node)
      }
    } else {
      return node.code
    }
  }
  const nodes = parse(moa)
  infer(nodes)
  const go = nodes.map(gen).join("\n\n")
  return `package main
import "fmt"

type MoaIO struct {
  puts func(...any)
}

var io = MoaIO{
  puts: func(a ...any) {
    fmt.Println(a...)
  },
}

${go}`
}

function main() {
  const go = compile(fs.readFileSync(process.argv[2] || "/dev/stdin", "utf-8"))
  fs.writeFileSync("/tmp/moa.go", go)
  console.log(child_process.execSync("go run /tmp/moa.go", {encoding: "utf-8"}))
}

main()
