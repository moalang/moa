const log = x => { console.log(x); return x }
const trace = x => { console.warn({trace: x}); return x }
const warn = x => { console.warn({warn: x}); return x }
function debug(x) { return warn(JSON.stringify(x)) }
function append(x, y) {
  let z = x || []
  z.push(y)
  return z
}
function define_global_prop(name, body) {
  return "Object.defineProperty(Object.prototype, '" + name + "', {" +
    "configurable: true," +
    "get() { return " + body + " }" +
  "})"
}
function ellipsis(s) {
  return s.length <= 100 ? s : s.slice(0, 100 - 3) + " ..."
}
function guard(cond, ret, message) {
  return cond ? ret : error(message)
}
function parse(src) {
  src = src.trim() + "\n"
  let depth = 0
  let pos = 0
  function parse_top() {
    return sepby1(parse_def, () => reg(/( *\n)+/))
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
    const s0 = many1(() => serial(read_white_spaces, parse_step))
    const s1 = s0.join(",\n")
    const s2 = s1.replace(/(\w+) *(?:<-|:=) *([^\n,]+)/g, '$1 = ($2)()')
    reg(/\s*}/)
    return "(() => (" + s2 + "))"
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
      return "(_ret = " + exp + ",true) && " + branch.join("") + "error('Does not match')"
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
        return "try_(() => " + l + ", () => " + r + ")"
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
      () => many(f, append(acc, f())),
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

function prepare() {
  function install_(obj, name, f) {
    Object.defineProperty(obj.prototype, name, {
      configurable: true,
      get: function() { return f(this) }
    })
  }
  function call_(f) {
    return typeof(f) === "function" ? call_(f()) : f
  }
  this.error = x => {
    const e = new Error("error: " + x)
    e.isMoa = true
    throw(e)
  }
  install_(Array, 'head', x => guard(x.length > 0, x[0], "out of index"))
  install_(Array, 'tail', x => x.slice(1))
  install_(Array, 'n0', x => x[0])
  install_(Array, 'n1', x => x[1])
  install_(Array, 'n2', x => x[2])
  Array.prototype.contains = function(x) { return this.indexOf(x) !== -1 }
  Array.prototype.nth = function(n) {
    return n >= this.length ? error("out of index") : this[n]
  }
  Array.prototype.append = function(x) { return this.concat(x) }
  String.prototype.nth = function(n) {
    return n >= this.length ? error("out of index") : this[n]
  }
  install_(String, 'to_i', parseInt)
  install_(String, 'to_a', x => x.split(""))
  install_(Number, 'to_s', String)
  this.try_ = function(l, r) {
    try {
      return call_(l)
    } catch (e) {
      if (e.isMoa) {
        return call_(r)
      }
      throw e
    }
  }
}

function run(js) {
  try {
    return eval(js)
  } catch (e) {
    if (!e.isMoa) {
      warn("failed to eval: " + js)
    }
    return e.message
  }
}

function test() {
  function t(expect, main) {
    const defs = Array.from(arguments).slice(2)
    const src = "main = " + main + (defs || []).map(x => "\n" + x).join("")
    const js = parse(src)
    const fact = run(js + "\n" + "call_(main)")
    if (JSON.stringify(expect) === JSON.stringify(fact)) {
      log("ok: " + JSON.stringify(fact))
    } else {
      log("expect: " + JSON.stringify(expect))
      log("  fact: " + JSON.stringify(fact))
      log("    js: " + js.split("\n").join("\n      | "))
      log("   moa: " + src.split("\n").join("\n      | "))
    }
  }
  log("---( basic pattern )---------")
  // value(4)
  t(1, "1")
  t("hello world", "\"hello world\"")
  t(true, "true")
  t(false, "false")
  t(true, "1 == 1")
  t(1, "inc(0)", "inc a = a + 1")
  t(2, "id(2)", "id = x => x")
  t(3, "add(1 2)", "add a b = a + b")
  // exp(8)
  t(4, "1 + 3")
  //t(4, "9 / 2")
  t(5, "a", "a = 5")
  t(6, "(x => x)(6)")
  t(7, "true\n| 7\n| 8")
  t(8, "false\n| 7\n| 8")
  t(true, "1\n| 1 = true\n| 2 = false")
  t(false, "3\n| 1 = true\n| _ = false")
  t(1, "ab.a\n| a = 1\n| b = 2", "ab:|\n  a\n  b")
  t(2, "ab.b\n| a = 1\n| b = 2", "ab:|\n  a\n  b")
  t(9, "{ 9 }")
  t(9, "{ e <- f\ne }", "f = { 9 }")
  t(9, "{ e = f\n9 }", "f = { error(\"ignore\") }")
  t("error: fail", "{ e <- f\ne }", "f = { error(\"fail\") }")
  t(9, "{ e <- f } ||| 9", "f = { error(\"fail\") }")
  // container(5)
  t([1], "[1]")
  t([1, 2], "[1 2]")
  t([1, 2, 3], "[1 2 3]")
  t(2, "(1, 2).1")
  t(3, "ab.a(3).x", "ab:|\n  a x int\n  b y int")
  t(4, "s(4 0).n", "s:\n  n int\n  m int")
  // error(2)
  t("error: failed", "f(1)", "f x = error(\"failed\")")
  t(2, "error(\"failed\") ||| 2")
  // built-in
  t(1, "\"01\".to_i")
  t("1,2,3", "[1 2 3].map(x => x.to_s).join(\",\")")
  t(1, "[1].nth(0)")
  t([1, 5], "[1 (2 + 3)]")
  t("i", "\"hi\".nth(1)")
  t(5, "(1, (2 + 3)).1")
  log("---( complex pattern )---------")
  // value(4)
  t('\"', '"\\""')
  t(1, "call(() => 1)", "call f = f()")
  t(2, 'call("\\"" "\\"")', "call a b = 2")
  // exp(8)
  t(3, "a", "c = 1\nb n = n + c\na = b(2)")
  t(4, "c + 2", "a =\n  1\n  2\nb =\n  a\n  a\nc = b")
  t(5, "f + 4", "f =\n  v = 1\n  v")
  t(6, "5 + g(1 \"a\" f(2) 3)", "f x = x\ng a b c d = a")
  t(7, "1 + sum([1 2 3])", "sum xs = (f acc xs = f(acc + xs.head xs.tail) ||| acc)(0 xs)")
  t(1, "f(1)", "f x = x\n| 1 = 1\n| _ = 2")
  t(8, "g(f(8) 2 g(3) f(4))", "f x = x\ng a b c d = a")
  // container(5)
  t(5, "(1, (2 + 3)).1")
  t([1, 5], "[1 (2 + 3)]")
  t(4, "ab.b(1).y + ab.b(2 3).z", "ab:|\n  a x int\n  b y int, z int")
  t(3, "f(ab.a) + f(ab.b)", "ab:|\n  a\n  b\nf x = x\n| a = 1\n| b = 2")
  //t(9, "s(8).incr",  "s:\n  n int\n  incr { n += 1 }")
  t(9, "s(8).incr",  "s:\n  n int\n  incr = { n += 1 }")
  t(10, "s(10).f2", "s:\n  n int\n  f1 =\n    n\n  f2 =\n    f1")
  t(11, "10 + s(1).f", "s:\n  n int\n  f =\n    v = n\n    v")
  // error(2)
  t("message", "calc", "f x = error(x)\ncalc =\n  r y = f(y) ||| y\n  r(\"message\")")
  // indent
  t(1, "p.n", "p:\n  m =\n    n\n  n =\n    1")
  t(2, "or(1 2)", "or l r =\n  a = l\n  alt x = 1; x\n  2 ||| alt(1)")
  log("done")
}

function compile() {
  const fs = require("fs")
  const src = fs.readFileSync(process.stdin.fd, "utf8").trim()
  const js = parse(src)
  const code = [
    prepare.toString(),
    js,
    "prepare()",
    "const ret = compile(" + JSON.stringify(src) + ")",
    "console.log(ret)"
  ].join("\n\n")
  return code
}

const cmd = process.argv[2]
if (cmd === "test") {
  prepare()
  test()
} else if (cmd === "html") {
  log("<html><script>function runDebug() {" + compile() + "}</script><body onload=runDebug()><button onclick=runDebug()>Debug</button></body></html>")
} else {
  log(compile())
}
