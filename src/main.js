const log = x => { console.log(x); return x }
const trace = x => { console.warn({trace: x}); return x }
const warn = x => { console.warn({warn: x}); return x }
function debug(x) { return warn("| " + JSON.stringify(x)) }
function append(x, y) {
  let z = x || []
  z.push(y)
  return z
}
function parse(src) {
  let pos = 0
  function parse_top() {
    return sepby1(
      () => or(parse_func, parse_type, parse_stmt),
      () => reg(/( *\n)+/))
  }
  function parse_func() {
    const id = read_id()
    const args = many(read_id).join(",")
    reg(/^ *= +/)
    const stmt = parse_stmt()
    return "const " + id + " = (" + args + ") => " + stmt
  }
  function parse_stmt() {
    const exp = parse_step()
    const branch = many(() => {
      reg(/^\n\| /)
      const val = parse_unit()
      eq(" = ")
      const ret = parse_exp()
      const cond = val === "_" ? "true" :
        (typeof val === "string" && val.match(/^[a-z]/) ? "_ret._tag === '" + val + "'" :
        "_ret === " + val)
      return "if (" + cond + ") {\n" + ret + "\n} else "
    })
    if (branch.length > 0) {
      return "let _ret = " + exp + ";" + branch.join("") + "\n { throw new Error('Does not match') }"
    }
    const fork = many(() => serial(() => reg(/^\n\| /), parse_exp))
    if (fork.length > 0) {
      const t = fork[0]
      const f = fork[1]
      return " if (" + exp + ") {" + t + "} else {" + f + "}"
    }
    return exp
  }
  function parse_step() {
    return parse_exp()
  }
  function parse_exp() {
    const exp = or(
      () => {
        const args = or(() => sepby1(read_id, () => eq(",")), () => [])
        eq(" => ")
        const stmt = parse_step()
        return "(" + args + ") => " + stmt
      },
      () => read_between("[", "]", () => "[" + many(parse_unit).join(",") + "]"),
      () => read_between("(", ")", () => "[" + sepby2(parse_unit, () => eq(",")).join(",") + "]"),
      () => and(parse_unit, read_op, parse_exp).join(""),
      parse_unit)
    const after = many(() => or(read_member, read_argv)).join("")
    return exp + after
  }
  function parse_unit() {
    return or(
      () => read_between("(", ")", () => "(" + parse_exp() + ")"),
      read_id,
      parse_value)
  }
  function parse_value() {
    return or(
      () => reg(/^ *(\d+)/, parseInt),
      () => reg(/^ *"[^"]*"/, eval),
      () => reg(/^ *(true|false)/, x => x[1] === "true")
    )
  }
  function parse_type() {
    const id = read_id()
    const type_args = many(read_id)
    const mark = type_args[type_args.length - 1]
    eq(":")
    if (mark === "enum") {
      const tags = many(() => {
        read_indent()
        const tag = read_id()
        const args = sepby(
          () => and(read_id, read_type)[0],
          () => eq(",")).join(",")
        if (args) {
          return tag + ": (" + args + ") => { return {_tag: '" + tag + "'," + args + "} }"
        } else {
          return tag + ": {_tag: '" + tag + "'}"
        }
      })
      if (tags.length > 0) {
        return "const " + id + " = {\n  " + tags.join(",\n  ") + "\n}"
      } else {
        throw new Error("failed to parse enum body")
      }
    }
    throw err("class does not support")
  }
  // consumer
  function read_id() {
    return reg(/^ *([a-zA-Z_][a-zA-Z0-9_]*)/)[1]
  }
  function read_type() {
    return read_id()
  }
  function read_stmt() {
    return reg(/^ *[^\n]+/)
  }
  function read_op() {
    return reg(/^ *([+\-*/:]=?|==)/)[1]
  }
  function read_member() {
    const m = reg(/^\.[a-zA-Z_0-9]*/)[0]
    return m[1].match(/[0-9]/) ? m.replace(".", ".n") : m
  }
  function read_argv() {
    return read_between("(", ")", () => "(" + many(parse_step) + ")")
  }
  function read_between(l, r, m) {
    return between(() => eq(l), () => eq(r), m)
  }
  function read_indent() {
    return reg(/^\n(?:  )+/)
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
    const extra = "\n   src: " + src + "\nremain: " + remain()
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
    return [x].concat(xs[xs.length - 1])
  }
  function sepby2(f, s) {
    const x = f()
    const xs = many1(() => serial(s, f))
    return [x].concat(xs[xs.length - 1])
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
  function reg(r, f) {
    f = f || (x => x)
    let m = src.slice(pos).match(r)
    if (!m) {
      throw err("miss match regexp: " + r + "\nremain: " + remain())
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
  const js = parse_top().join("\n").trim()
  if (remain().length !== 0) {
    throw err("Failed parsing remaining: `" + remain() + "`")
  }
  return js
}

function prepare() {
  this.error = x => {
    const e = new Error("error: " + x)
    e.isMoa = true
    throw(e)
  }
  function guard(cond, ret, msg) { if (cond) { return ret } else { error(msg) } }
  function install(obj, name, f) {
    Object.defineProperty(obj.prototype, name, {get: function() { return f(this) }})
  }
  warn({xxx:[1,2].n1})
  install(Array, 'head', x => guard(x.length > 0, x[0], "out of index"))
  install(Array, 'tail', x => x.slice(1))
  install(Array, 'n0', x => x[0])
  install(Array, 'n1', x => x[1])
  install(Array, 'n2', x => x[2])
  Array.prototype.contains = function(x) { return this.indexOf(x) !== -1 }
  Array.prototype.nth = function(n) {
    if (n >= this.length) {
      error("out of index")
    } else {
      return this[n]
    }
  }
  Array.prototype.append = function(x) { return this.concat(x) }
  String.prototype.nth = function(n) {
    if (n >= this.length) {
      error("out of index")
    } else {
      return this[n]
    }
  }
  install(String, 'to_i', parseInt)
  install(String, 'to_a', x => x.split(""))
  install(Number, 'to_s', String)
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
  function t(expect, src) {
    const js = parse(src)
    const fact = run(js)
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
  t(2, "inc a = a + 1\ninc(1)")
  t(3, "add a b = a + b\nadd(1 2)")
  // exp(8)
  t(4, "1 + 3")
  //t(4, "9 / 2")
  t(5, "a = 5\na()")
  t(6, "(x => x)(6)")
  t(7, "true\n| 7\n| 8")
  t(8, "false\n| 7\n| 8")
  t(true, "1\n| 1 = true\n| 2 = false")
  t(false, "3\n| 1 = true\n| _ = false")
  t(1, "ab enum:\n  a\n  b\nab.a\n| a = 1\n| b = 2")
  t(2, "ab enum:\n  a\n  b\nab.b\n| a = 1\n| b = 2")
  // container(5)
  t([1], "[1]")
  t([1, 2], "[1 2]")
  t(2, "(1, 2).1")
  t(3, "ab enum:\n  a x int\n  b y int\nab.a(3).x")
  t(1, "s class:\n  n int\n  m int\ns(1 2).n")
  // error(2)
  t("error: failed", "f x = error(\"failed\")\nf(1)")
  t(2, "error(\"failed\") | 2")
  // built-in
  t(1, "\"01\".to_i()")
  t("1,2,3", "[1 2 3].map(x=>x.to_s()).join(\",\")")
  t(1, "[1].nth(0)")
  t(5, "[1 (2 + 3)].nth(1)")
  t("i", "\"hi\".nth(1)")
  t(5, "(1, 2 + 3).1")
  log("---( complex pattern )---------")
  // exp(8)
  t(3, "c = 1\nb n = n + c()\na = b(2)\na()")
  t(2, "a =\n  1\n  2\nb = a(); a()\nc = b()\nc()")
  t(1, "f =\n  v = 1\n  v\nf()")
  t(1, "f x = x\ng a b c d = a\ng(1 \"a\" f(2) 3)")
  t(6, "sum xs = (f acc xs = f(acc + xs.head() xs.tail()) | acc)(0 xs)\nsum([1 2 3])")
  t(3, "f = 1 | 2; 3\nf()")
  t(1, "f x = x\n| 1 = 1\n| _ = 2\nf(1)")
  t(8, "f x = x\ng a b c d = a\ng(f(8) 2 g(3) f(4))")
  // container(5)
  t(5, "(1, 2 + 3).1")
  t([1, 5], "[1 (2 + 3)]")
  t(4, "ab enum:\n  a x int\n  b y int, z int\nab.b(1).y + ab.b(2 3).z")
  t(3, "ab enum:\n  a\n  b\nf x = x\n| a = 1\n| b = 2\nf(ab.a) + f(ab.b)")
  t(9, "s class:\n  n int\n  incr = n += 1\n  incr2 = incr()\n  mul x =\n    n := n * x\nt s(1)\nt.incr()\nt.incr2()\nt.mul(3)")
  t(10, "s class:\n  n int\n  f1 =\n    n\n  f2 =\n    f1()\ns(10).f2()")
  t(1, "s class:\n  n int\n  f =\n    v = n\n    v\ns(1).f()")
  // error(2)
  t("message", "f x = error(x)\ncalc =\n  r y = f(y) | y\n  r(\"message\")\ncalc()")
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
