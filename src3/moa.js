function define_global_prop(name, body) {
  return "Object.defineProperty(Object.prototype, '" + name + "', {" +
    "configurable: true," +
    "get() { return " + body + " }" +
  "})"
}
function ellipsis(s) {
  return s.length <= 100 ? s : s.slice(0, 100 - 3) + " ..."
}
function parse(src) {
  src = src.trim() + "\n"
  let depth = 0
  let pos = 0
  function parse_top() {
    return sepby1(parse_top_line, () => reg(/( *\n)+/))
  }
  function parse_import() {
    const name = reg(/^- (\w+)/)[1]
    return "const " + name + " = __moa__." + name
  }
  function parse_top_line() {
    return or(parse_import, parse_def)
  }
  function parse_def() {
    const id = read_id()
    const args = many(read_id)
    const mark = reg(/^ *(:\||[:=\n])/)[1]
    if (mark === ":") {
      return def_class(id, args)
    } if (mark === ":|") {
      return def_enum(id, args)
    } else if (mark === "=") {
      return def_func(id, args)
    } else {
      throw err("unkown type definition")
    }
  }
  function def_enum(id, args) {
    const tags = many(() => {
      read_indent()
      const tag = read_id()
      const args = read_attrs()
      if (args) {
        return tag + ": (" + args + ") => { return {_tag: '" + tag + "'," + args + "} }"
      } else {
        return tag + ": {_tag: '" + tag + "'}"
      }
    })
    return "const " + id + " = {\n  " + tags.join(",\n  ") + "\n}"
  }
  function def_class(id, args) {
    const names = many(() => serial(read_indent, read_attr))
    const methods = many(() => serial(read_indent, def_method))
    const body = names.concat(methods.map(x => x.id)).join(",")
    const defs = methods.map(x => "\n  " + x.body).join("")
    if (names.length === 0) {
      return define_global_prop(id, "(function(){ " + defs + "\nreturn {" + body + "}" + "})()")
    } else {
      return "function " + id + "(" + names.join(",") + ") {" + defs + "\n  return {\n  " + body + "\n} }"
    }
  }
  function def_method() {
    const id = read_id()
    const args = many(read_id)
    eq(" =")
    const exp = or(() => indent(parse_seq), parse_step)
    const body = make_func(id, args, exp)
    return {id, body}
  }
  function def_func(id, args) {
    const body = or(parse_seq, parse_step)
    return make_func(id, args, body)
  }
  function make_func(id, args, body) {
    if (args.length === 0) {
      return define_global_prop(id, body)
    } else {
      return "function " + id + " (" + args + ") { return " + body + " }"
    }
  }
  function parse_stmt() {
    reg(/ *{/)
    const lines = many1(() => serial(read_white_spaces, parse_step)).map(x =>
      x.replace(/(\w+) *(?:<-|:=) *([^\n,]+)/g, 'let $1 = __moa__.call($2)'))
    reg(/\s*}/)
    lines[lines.length - 1] = "return " + lines[lines.length - 1]
    return "function() {\n  " + lines.join("\n  ") + "\n}"
  }
  function parse_seq() {
    return "(" + many1(() => serial(read_indent, parse_step)).join(",\n") + ")"
  }
  function parse_step() {
    let exp = parse_exp()
    if (exp.match(/^([a-z]\w*)$/)) {
      if (check(/^( \w)* *=/)) {
        const id = exp
        const args = many(read_id).join(",")
        eq(" =")
        const body = or(parse_seq, parse_step)
        if (args) {
          exp = id + " = (" + args + ") => " + body
        } else {
          exp = id + " = " + body
        }
      }
    }
    const branch = many(() => {
      reg(/^\n\| /)
      const val = parse_unit()
      eq(" = ")
      const ret = parse_exp()
      const cond = val === "_" ? "true" :
        (typeof val === "string" && val.match(/^[a-z]/) ? "_ret._tag === '" + val + "'" :
        "_ret === " + val)
      return cond + " ? " + ret + " : "
    })
    if (branch.length > 0) {
      return "(_ret = " + exp + ",true) && " + branch.join("") + "__moa__.error('Does not match')"
    }
    const fork = many(() => serial(() => reg(/^\n\| /), parse_exp))
    if (fork.length > 0) {
      const t = fork[0]
      const f = fork[1]
      return exp + " ? " + t + " : " + f
    }
    return exp
  }
  function parse_exp() {
    const exp = or(
      () => {
        const args = or(
          () => read_between("(", ")", () => sepby(read_id, () => eq(","))),
          () => sepby(read_id, () => eq(",")))
        eq(" => ")
        const stmt = parse_step()
        return "(" + args + ") => " + stmt
      },
      () => {
        const l = parse_unit()
        eq(" ||| ")
        const r = parse_exp()
        return "__moa__.attempt(() => " + l + ", () => " + r + ")"
      },
      () => {
        const l = parse_unit()
        const o = read_op()
        const r = parse_exp()
        if (o === ";") {
          return "(" + l + "," + r + ")"
        } else {
          return l + o + r
        }
      },
      parse_unit)
    const after = many(() => or(read_member, read_argv)).join("")
    return exp + after
  }
  function parse_unit() {
    const unit = or(
      parse_stmt,
      () => read_between("[", "]", () => "[" + sepby(parse_unit, () => reg(/ +/)).join(",") + "]"),
      () => read_between("(", ")", () => "[" + sepby2(parse_unit, () => eq(",")).join(",") + "]"),
      () => read_between("(", ")", () => "(" + parse_def().replace("const ", "") + ")"),
      () => read_between("(", ")", () => "(" + parse_exp() + ")"),
      read_id,
      parse_value)
    const after = many(() => or(read_member, read_argv)).join("")
    return unit + after
  }
  function parse_value() {
    return or(
      () => reg(/^ *(\d+)/, parseInt),
      () => reg(/^ *"(?:(?:\\")|[^"])*"/, eval),
      () => reg(/^ *(true|false)/, x => x[1] === "true")
    )
  }
  // consumer
  function read_id() {
    return reg(/^ *([a-zA-Z_][a-zA-Z0-9_]*)/)[1]
  }
  function read_type() {
    return read_id()
  }
  function read_attr() {
    return and(read_id, read_type)[0]
  }
  function read_attrs() {
    return sepby(read_attr, () => eq(",")).join(",")
  }
  function read_op() {
    return reg(/^ *([+\-*/:]=?|==|!=|<-|\|\||&&|=|;)/)[1]
  }
  function read_member() {
    const m = reg(/^\.[a-zA-Z_0-9]*/)[0]
    return m[1].match(/[0-9]/) ? m.replace(".", ".n") : m
  }
  function read_argv() {
    return between(() => eq("("), () => eq(")"), () => "(" + many(parse_step) + ")")
  }
  function read_between(l, r, m) {
    return between(
      () => { many(() => eq(" ")); return eq(l) },
      () => { many(() => eq(" ")); return eq(r) },
      m)
  }
  function read_white_spaces() {
    return reg(/[ \t\n]+/)
  }
  function read_indent() {
    return eq("\n  " + "  ".repeat(depth))
  }
  function indent(f) {
    try {
      depth += 1
      return f()
    } finally {
      depth -= 1
    }
  }
  // combinator
  function many(f, acc) {
    acc = acc || []
    return or(
      () => many(f, acc.concat(f())),
      () => acc)
  }
  function many1(f) {
    const v = f()
    return many(f, [v])
  }
  function err(message) {
    const extra = "\n   src: " + ellipsis(src) + "\nremain: " + ellipsis(remain())
    const e = new Error(message + extra)
    e.isParser = true
    return e
  }
  function sepby(f, s) {
    return or(() => sepby1(f, s), () => [])
  }
  function sepby1(f, s) {
    const x = f()
    const xs = many(() => serial(s, f))
    return [x].concat(xs)
  }
  function sepby2(f, s) {
    const x = f()
    const xs = many(() => serial(s, f))
    return [x].concat(xs)
  }
  function between(l, r, m) {
    return and(l, m, r)[1]
  }
  function or() {
    const bk = pos
    for (let i=0; i<arguments.length; i++) {
      const f = arguments[i]
      try {
        return f()
      } catch (e) {
        if (!e.isParser) {
          throw e
        }
        pos = bk
      }
    }
    throw err("not match in or combinator" +
      "\n- " + Array.from(arguments).map(x => x.toString()).join("\n- "))
  }
  function and() {
    return Array.from(arguments).map(x => x())
  }
  function serial() {
    return Array.from(arguments).map(x => x()).slice(-1)[0]
  }
  // matcher
  function check(r) {
    return src.slice(pos).match(r)
  }
  function reg(r, f) {
    f = f || (x => x)
    let m = src.slice(pos).match(r)
    if (!m) {
      throw err("miss match regexp: " + r + "\nremain: " + ellipsis(remain()))
    }
    if (m.index !== 0) {
      throw err("matched position should be 0 regexp: " + r)
    }
    const len = m[0].length
    if (len == 0) {
      throw err("invalid zero width matching regexp: " + r)
    }
    pos += len
    return f(m)
  }
  function eq(s) {
    if (src.slice(pos, pos + s.length) === s) {
      pos += s.length
      return s
    } else {
      throw err("does not match: " + s)
    }
  }
  function remain() {
    return src.slice(pos)
  }
  // do parsing
  const lines = parse_top()
  const js = lines.join("\n").trim()
  if (remain().trim().length !== 0) {
    const red = src.slice(0, pos)
    const line = red.split("\n").length
    const column = red.length - red.lastIndexOf("\n") + 1
    throw err("Failed parsing remaining: at " + line + ":" + column +
      "\n`" + ellipsis(remain()) + "`" +
      "\nlines: # " + lines.join("\n# "))
  }
  return js
}

function prepare(target) {
  function extend(obj, name, f) {
    Object.defineProperty(obj.prototype, name, {
      configurable: true,
      get: function() { return f(this) }
    })
  }
  function call(f) {
    return typeof(f) === "function" ? call(f()) : f
  }
  function error(x) {
    const e = new Error("error: " + x)
    e.isMoa = true
    throw(e)
  }
  function attempt(l, r) {
    try {
      return call(l)
    } catch (e) {
      if (e.isMoa) {
        return call(r)
      }
      throw e
    }
  }
  function guard(cond, ret, message) {
    return cond ? ret : error(message)
  }
  extend(Array, 'head', x => guard(x.length > 0, x[0], "out of index"))
  extend(Array, 'tail', x => x.slice(1))
  extend(Array, 'n0', x => x[0])
  extend(Array, 'n1', x => x[1])
  extend(Array, 'n2', x => x[2])
  Array.prototype.contains = function(x) { return this.indexOf(x) !== -1 }
  Array.prototype.nth = function(n) {
    return n >= this.length ? error("out of index") : this[n]
  }
  String.prototype.nth = function(n) {
    return n >= this.length ? error("out of index") : this[n]
  }
  extend(String, 'to_i', parseInt)
  extend(String, 'to_a', x => x.split(""))
  extend(Number, 'to_s', String)
  Function.prototype.fmap = function(f) { return () => f(this()) }

  const fs = require('fs')
  target.error = error
  target.__moa__ = {
    attempt,
    call,
    error,
    io: {
      write: s => process.stdout.write(call(s).toString()),
      print: s => console.log(call(s)),
      stdin: {
        read: () => fs.readFileSync('/dev/stdin', 'utf8')
      },
    },
  }
}

function compile(src) {
  return [
    prepare.toString(),
    "(function() {",
    "prepare(this)",
    parse(src),
    " __moa__.call(main)",
    "})()"
  ].join("\n")
}
exports.compile = compile
exports.parse = parse
exports.prepare = prepare

if(require.main === module) {
  const fs = require("fs")
  const src = fs.readFileSync(process.stdin.fd, "utf8").trim()
  console.log(compile(src))
}
