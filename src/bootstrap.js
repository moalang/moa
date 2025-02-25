"use strict"
// echo 'def main: io.puts "hello"' | node bootstrap.js | grep hello
const fs = require("node:fs")
const child_process = require("node:child_process")

function assert(cond, ...a) {
  if (!cond) {
    throw new Error(a.map(x => JSON.stringify(x, null, 2)).join(" "))
  }
}

function isOp2(s) {
  return "+-*/%<>!=^|&".includes(s[0])
}

function tokenize(moa) {
  let lineno = 1
  let offset = 0
  let indent = 0
  const tokens = []
  for (const code of moa.split(/([ \n]+|[()\[\]{}]|[:.+\-*/%!=^|&?]+|"[^"]*"|[ \n]+)/)) {
    if (code.trim()) {
      tokens.push({code, lineno, offset, indent})
    }
    offset += code.length
    lineno += code.split("\n").length - 1
    indent = code.includes("\n") ? code.split("\n").at(-1).length : indent
  }
  return tokens
}

function parse(moa) {
  const tokens = tokenize(moa)
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
    if (token.code === "(") {
      return suffix(many(t => t.code === ")" ? (pos++, null) : bottom()))
    } else if (token.code === "[") {
      return suffix([{code: "array"}].concat(many(t => t.code === "]" ? (pos++, null) : bottom())))
    } else if (token.code === ":") {
      if (token.lineno === tokens[pos].lineno) {
        return [statement()]
      } else {
        const indent = tokens[pos].indent
        const nodes = many(t => tokens[pos].indent === indent && statement())
        assert(nodes.length > 0, "Empty indent", pos, tokens.length)
        return nodes
      }
    } else {
      return suffix(token)
    }
  }
  function many(f) {
    const nodes = []
    while (pos < tokens.length) {
      if (tokens[pos].code === "#") {
        const lineno = tokens[pos].lineno
        while (pos < tokens.length && tokens[pos].lineno === lineno) {
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
  function statement() {
    const lineno = tokens[pos].lineno
    const nodes = many(t => t.lineno === lineno && !closes.includes(tokens[pos].code) && bottom())
    assert(nodes.length > 0, "Empty line", pos, tokens.length, tokens.slice(pos))
    return nodes.length === 1 && Array.isArray(nodes[0]) ? nodes[0] : nodes
  }
  const top = many(statement)
  assert(pos >= tokens.length, "No reach end of token", pos, tokens.length)
  return top
}

function infer(nodes) {
  let typeVariableSequence = 0
  const newType = (name,generics) => generics ? ({name,generics}) : ({name})
  const newVar = () => ({name: (typeVariableSequence++).toString(), isvar: true})
  const toVariadic = t => ({...t, variadic: true})
  const tvoid = newType("void")
  const tany = ({name: "any"})
  const tv1 = newVar()
  const tbool = newType("bool")
  const tint = newType("int")
  const tstring = newType("string", [])
  const tarray = t => newType("array", [t])
  const props = {
    io: {
      puts: [toVariadic(tany), tvoid],
      args: [tarray(tstring)],
    },
    string: {
      size: [tint],
    },
    array: (t) => ({
      size: [tint],
      slice: [tint, toVariadic(tint), t],
      join: [tstring, tstring],
      push: [t.generics[0], t],
      at: [tint, t],
    }),
  }
  function prune(t) {
    return t.instance ? (t.instance = prune(t.instance)) : t
  }
  function unify(a, b) {
    a = prune(a)
    b = prune(b)
    if (a === b || a.name == "any" || b.name == "any") {
      // do nothing
    } else if (a.isvar) {
      a.instance = b
    } else if (b.isvar) {
      unify(b, a)
    } else {
      assert(Array.isArray(a) === Array.isArray(b), "Not function", a, b)
      if (Array.isArray(a)) {
        assert(a.length === b.length, "No match length of arguments", a, b)
        a.map((_, i) => unify(a[i], b[i]))
      } else {
        assert(a.name === b.name, "No unify", showType(a), showType(b))
        const ag = a.generics || []
        const bg = b.generics || []
        assert(ag.length === bg.length, {a, b})
        ag.map((_, i) => unify(ag[i], bg[i]))
      }
    }
    return a
  }
  function call(f, args) {
    f = prune(f)
    if (f.isvar) {
      return (f.instance = args.concat(newVar())).at(-1)
    } else {
      assert(Array.isArray(f), f)
      assert((f.length >= 2 && f.at(-2).variadic) || f.length === args.length + 1, "Not callable", f.map(showType), args.map(showType))
      for (let i=0; i<args.length; i++) {
        unify(f[i], args[i])
        if (f[i].variadic) {
          for (let j=i+1; j<args.length; j++) {
            unify(f[i], args[j])
          }
          break
        }
      }
      return f.at(-1)
    }
  }
  function inferWith(node, tenv) {
    function lookup(node) {
      assert(node.code in tenv, `No '${node.code}'`, node, Object.keys(tenv))
      return prune(tenv[node.code])
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
        } else if (head.code === "return") {
          return tail.length === 0 ? tvoid : inf(tail[0])
        } else if (head.code === "def") {
          const args = tail.slice(1, -1)
          const vars = args.map(newVar)
          const tenv2 = {...tenv, ...Object.fromEntries(args.map((arg,i) => [arg.code, vars[i]]))}
          const ret = inferTop(tail.at(-1), tenv2).at(-1)
          return unify(lookup(tail[0]), vars.concat([ret]))
        } else if (head.code === "dec") {
          const args = tail.slice(1, -1)
          const vars = args.map(newVar)
          const tenv2 = {...tenv, ...Object.fromEntries(args.map((arg,i) => [arg.code, vars[i]]))}
          return tenv[tail[0].code] = tail.at(-1).flatMap(x => x).map(x => inferWith(x, tenv2))
        } else if (head.code === "struct") {
          const id = tail[0].code
          const fields = tail.at(-1).map(line => line[0].code)
          const types = tail.at(-1).map(line => inf(line[1]))
          props[id] = Object.fromEntries(fields.map((_, i) => [fields[i], types[i]]))
          return unify(tenv[id], [...types, newType(id)])
        } else if (head.code === "let" || head.code === "var") {
          return tenv[tail[0].code] = inf(tail[1])
        } else if (head.code === ".") {
          const type = inf(tail[0])
          const id = tail[1].code
          const prop = props[type.name]
          assert(prop, "No type", type, id)
          const field = typeof prop === "function" ? prop(type)[id] : prop[id]
          assert(field, "No field", typeof prop, type, id)
          return field
        } else {
          const target = lookup(head)
          assert(tail.length === 0 || (Array.isArray(target) || target.isvar), "Not a function", target)
          const args = tail.map(inf)
          const ret = call(target, args)
          tenv[head.code].type = args.concat(ret)
          return ret
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
  function inferTop(nodes, tenv) {
    assert(Array.isArray(nodes), "No arary", typeof nodes)
    const definitions = "def struct let var".split(" ")
    for (const node of nodes.filter(node => definitions.includes(node[0].code))) {
      tenv[node[1].code] = newVar()
    }
    return nodes.map(node => inferWith(node, tenv))
  }
  function recPrune(node) {
    if (node.type) {
      node.type = prune(node.type)
      assert(!node.type.isvar, "A type is not inferred", node)
    }
    if (Array.isArray(node)) {
      node.map(recPrune)
    }
  }
  const troot = {
    "io": newType("io"),
    "array": (t => [t, tarray(t)])(toVariadic(tv1)),
    "+": [tv1, tv1, tv1],
    "int": tint,
    "string": tstring,
    "bool": tbool,
  }
  inferTop(nodes, troot)
  recPrune(nodes)
  //nodes.map(node => inferWith(node, troot))
}

function showType(o) {
  function show(o) {
    return Array.isArray(o) ? `${o.map(show).join(":")}` : o.instance ? showType(o.instance) : `${o.variadic ? "..." : ""}${o.name}${o.generics && o.generics.length ? "[" + o.generics.map(showType).join(" ") + "]" : ""}`
  }
  const m = new Map()
  return show(o).replace(/[0-9]+/g, n => m.get(n) || m.set(n, m.size+1).get(n))
}

function showNode(o) {
  return (Array.isArray(o) ? `(${o.map(showNode).join(" ")})` : o.code || JSON.stringify(o))
}

function showFull(o) {
  return (Array.isArray(o) ? `(${o.map(showFull).join(" ")})` : o.code || JSON.stringify(o)) + (o.type ? "@" + showType(o.type) : "")
}

function compile(moa) {
  const nodes = parse(moa)
  infer(nodes)
  const userTypes = nodes.filter(node => node[0].code === "struct").map(node => node[1].code)
  const embededTypes = "void bool int float string array dict".split(" ")
  function goType(type) {
    const t = type.name
    return type.instance ? goType(type.instance) :
      Array.isArray(type) ? (a => `func(${a.slice(0, -1)}) ${a.at(-1)}`)(type.map(goType)) :
      t === "void"  ? "" :
      t === "int"   ? "int64" :
      t === "float" ? "float64" :
      t === "array" ? `[]${goType(type.generics[0])}` :
      t === "dict"  ? `dict[${goType(type.generics[0])}]${goType(type.generics[1])}` :
      [
        type.variadic ? "..." : "",
        type.name,
        type.generics && type.generics.length ? `[${type.generics.map(goType)}]` : ""
      ].join("")
  }
  function gen(node) {
    if (Array.isArray(node)) {
      const head = node[0]
      const tail = node.slice(1)
      if (Array.isArray(head)) {
        const baseType = head[1].type.name
        if (head[0].code === "." && baseType === "io") {
          return `${baseType}_${head[2].code}(${tail.map(gen)})`
        } else if (head[0].code === "." && baseType === "array" && head[2].code === "push") {
          assert(head[1].code, "array_push violation")
          const id = head[1].code
          return `${id} = append(${id}, ${gen(tail[0])})`
        } else if (head[0].code === "." && embededTypes.includes(baseType)) {
          const args = [head[1]].concat(tail)
          return `${baseType}_${head[2].code}(${args.map(gen)})`
        } else {
          return `${gen(head)}(${tail.map(gen)})`
        }
      } else if (head.code === "return") {
        return tail.length === 0 ? "return" : `return ${gen(tail[0])}`
      } else if (head.code === "def") {
        const types = node.type.map(goType)
        const args = tail.slice(1, -1).map((node, i) => node.code + " " + types[i])
        const body = tail.at(-1).map(gen).join("\n")
        return `func ${tail[0].code}(${args}) ${types.at(-1)} {${body}}`
      } else if (head.code === "dec") {
        return `// ${node}`
      } else if (head.code === "struct") {
        const id = tail[0].code
        const fields = tail.at(-1)
        const body = fields.map(([field, type]) => field.code + " " + goType(type.type) ).join(";")
        return `type ${id} struct { ${body} }`
      } else if (head.code === "let" || head.code === "var") {
        return `${tail[0].code} := ${gen(tail[1])}`
      } else if (head.code === ".") {
        const baseType = tail[0].type.name
        if (baseType === "io") {
          return `io_${tail[1].code}`
        } else {
          return gen(tail[0]) + head.code + gen(tail[1])
        }
      } else if ("+-*/%!=^|&?<>.".includes(head.code[0])) {
        return gen(tail[0]) + head.code + gen(tail[1])
      } else {
        const generics = tail.length === 0 && node.type.generics && node.type.generics.length ? "[" + node.type.generics.map(goType) + "]" : ""
        if (userTypes.includes(gen(head))) {
          return `${gen(head)}${generics}{${tail.map(gen)}}`
        } else {
          return `${gen(head)}${generics}(${tail.map(gen)})`
        }
      }
    } else {
      const s = node.code
      return s.startsWith('"') ? JSON.stringify(s.slice(1, -1)) : s
    }
  }
  return nodes.map(gen).join("\n") + "\n"
}

const runtime = fs.readFileSync(__dirname + "/runtime.go", "utf-8")
if (process.argv[2] === "selfboot") {
  const moa = fs.readFileSync(__dirname + "/moa.moa", "utf-8")
  const prefix = `package main
import (
  _ "fmt"
)`
  fs.writeFileSync(`${__dirname}/moa.go`, prefix + "\n" + compile(moa))
  console.log(child_process.execSync(`go build -o /tmp/moa moa.go runtime.go main.go`, {
    cwd: __dirname,
    encoding: "utf-8"
  }))
} else {
  const moa = fs.readFileSync("/dev/stdin", "utf-8")
  const go = runtime + compile(moa)
  fs.writeFileSync("/tmp/moa.go", go)
  console.log(child_process.execSync(`go run /tmp/moa.go ${process.argv.slice(2).join(" ")}`, {encoding: "utf-8"}))
}
