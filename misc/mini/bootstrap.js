"use strict"
// echo 'def main: io.puts "hello"' | node bootstrap.js | grep hello
const fs = require("node:fs")
const child_process = require("node:child_process")

function assert(cond, ...a) {
  if (!cond) {
    throw new Error(a.map(x => JSON.stringify(x)).join(" "))
  }
}

function isOp2(s) {
  return "+-*/%<>!=^|&".includes(s[0])
}

function tokenize(moa) {
  let line = 1
  let offset = 0
  let indent = 0
  const tokens = []
  for (const code of moa.split(/([ \n]+|[()\[\]{}]|[:.+\-*/%!=^|&?]+|"[^"]*"|[ \n]+)/)) {
    if (code.trim()) {
      tokens.push({code, line, offset, indent})
    }
    offset += code.length
    line += code.split("\n").length - 1
    indent = code.includes("\n") ? code.split("\n").at(-1).length : indent
  }
  return tokens
}

function parse(moa) {
  const tokens = tokenize(moa)
  const opens = "{([".split("")
  const closes = "])}".split("")
  let pos = 0
  function bottom() {
    const token = tokens[pos++]
    function suffix(node) {
      const next = tokens[pos]
      const prev = tokens[pos-1]
      if (next && isOp2(next.code)) {
        pos++ // consume binary operator
        return suffix([next, node, bottom()])
      } else if (next && next.code === ".") {
        pos++ // consume dot
        return suffix([next, node, tokens[pos++]])
      } else if (prev && next && prev.offset + prev.code.length === next.offset && next.code === "(") {
        pos++ // consume closed open parenthesis
        return suffix([node].concat(many(t => t.code === ")" ? (pos++, null) : bottom())))
      } else {
        return node
      }
    }
    const index = opens.findIndex(code => code === token.code)
    if (index >= 0) {
      return suffix(many(t => t.code === closes[index] ? (pos++, null) : bottom()))
    } else if (token.code === ":") {
      const indent = tokens[pos].indent
      const nodes = many(t => tokens[pos].indent === indent && line())
      assert(nodes.length > 0, "Empty indent", pos, tokens.length)
      return nodes
    } else {
      return suffix(token)
    }
  }
  function many(f) {
    const nodes = []
    while (pos < tokens.length) {
      if (tokens[pos].code === "#") {
        const line = tokens[pos].line
        while (pos < tokens.length && tokens[pos].line === line) {
          pos++
        }
      } else {
        const node = f(tokens[pos])
        if (!node) {
          break
        }
        nodes.push(node)
      }
    }
    return nodes
  }
  function line() {
    const line = tokens[pos].line
    const nodes = many(t => t.line === line && !closes.includes(tokens[pos].code) && bottom())
    assert(nodes.length > 0, "Empty line", pos, tokens.length, tokens.slice(pos))
    return nodes.length === 1 && Array.isArray(nodes[0]) ? nodes[0] : nodes
  }
  const top = many(line)
  assert(pos >= tokens.length, "No reach end of token", pos, tokens.length)
  return top
}

function infer(nodes) {
  let typeVariableSequence = 0
  const newType = (name,generics=[],props={}) => ({name,generics,props})
  const newVar = () => ({name: (typeVariableSequence++).toString(), isvar: true})
  const toVariadic = t => ({...t, variadic: true})
  const tvoid = newType("tvoid")
  const tany = ({name: "any", isvar: true})
  const tv1 = newVar()
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
      assert(a.name === b.name, "No unify", showType(a), showType(b))
      assert(a.generics.length === b.generics.length, {a, b})
      a.generics.map((ag, i) => unify(ag, b.generics[i]))
    }
  }
  function call(f, args) {
    assert(Array.isArray(f), f)
    assert((f.length >= 1 && f.at(-2).variadic) || f.length === args.length + 1, "Not callable", f.map(showType), args.map(showType))
    args.map((a, i) => unify(f[i], a))
    return f.at(-1)
  }
  function inferWith(node, tenv) {
    function lookup(node) {
      assert(node.code in tenv, node.code, Object.keys(tenv))
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
        } else if (head.code === "let" || head.code === "var") {
          return tenv[tail[0].code] = inf(tail[1])
        } else if (head.code === ".") {
          const prop = inf(tail[0]).props[tail[1].code]
          assert(prop, node)
          return prop
        } else {
          const id = head.code
          assert(id in tenv, "No id", id, Object.keys(tenv))
          if (tail.length === 0) {
            return tenv[id]
          } else {
            return call(tenv[id], tail.map(inf))
          }
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
      puts: [toVariadic(tany), tvoid],
    }),
    "+": [tv1, tv1, tv1],
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
        const body = tail.at(-1).map(gen).join("\n")
        return `func ${tail[0].code}(${args}) {${body}}`
      } else if (head.code === "let" || head.code === "var") {
        return `${tail[0].code} := ${gen(tail[1])}`
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
  const go = nodes.map(gen).join("\n") + "\n"
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

const go = compile(fs.readFileSync("/dev/stdin", "utf-8"))
fs.writeFileSync("/tmp/moa.go", go)
console.log(child_process.execSync(`go run /tmp/moa.go ${process.argv.slice(2).join(" ")}`, {encoding: "utf-8"}))
