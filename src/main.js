const log = x => { console.log(x); return x }
const warn = x => { console.warn({x}); return x }
function debug(x) { return warn("| " + JSON.stringify(x)) }
function append(x, y) {
  let z = x || []
  z.push(y)
  return z
}
function swap(s, r, f) {
  let m
  while (m = s.match(r)) {
    s = s.replace(m[0], f(m))
  }
  return s
}
function parse(src) {
  let pos = 0
  let depth = 0
  let def_mode = 0
  let seq_mode = 1

  // parser
  function parse_top() {
    return sep_by1(parse_line(0, def_mode), read_indent(0)).and(x => x.join(";\n"))
  }
  function parse_line(offset, mode) {
    return first([
      parse_enum(),
      parse_class(),
      parse_func(offset, mode),
      parse_var(),
      parse_fork()])
  }
  function parse_var() {
    return reg(/^(\w+) (\S+)$/m).and(m =>
      "let " + m[1] + " = " + fix_exp(m[2]))
  }
  function parse_func(offset, mode) {
    return reg(/^( *)(\w+)((?: \w+)+)? *=(?!=)/m).and(m => {
      const id = m[2]
      const args = (m[3] || "").trim().split(" ").join(", ")
      const in_seq = mode === seq_mode
      const next_mode = (mode === def_mode && src[pos] === "\n") ? seq_mode : mode
      return parse_body(offset, next_mode).and(exp =>
        (in_seq && args.length === 0)
        ? "let " + id + " = " + exp
        : "const " + id + " = (" + args + ") => " + exp)
    })
  }
  function parse_body(offset, mode) {
    return many1(read_indent(1 + offset).and(parse_line(offset, mode))).or(parse_line(offset, mode)).and(to_block)
  }
  function parse_fork() {
    return parse_exp().and(x => first([
      read_fork_match(x),
      read_fork_bool(x),
      x
    ]))
  }
  function parse_fork_bool() {
    return read_fork_bool()
  }
  function parse_exp() {
    return reg(/[^\n]+/).and(x => fix_exp(x[0]))
  }
  function parse_enum() {
    return reg(/^(\w+)(?: \w+)*? enum:/).and(x =>
      many1(reg(/^\n  (\w+)((?: \S+)*)/)).and(xs =>
        to_enum(x[1], xs)))
  }
  function to_enum(id, enums) {
    return "const " + id + " = {\n" +
      enums.map(x => to_enum_tag(x[1], x[2])).join(",") +
      "\n}"
  }
  function to_enum_tag(tag, attrs) {
      return tag + ": " + to_enum_new(tag, attrs)
  }
  function to_enum_new(id, line) {
    if (line) {
      const attrs = line.trim().split(", ").map(x => x.split(" ", 2)[0]).join(", ")
      if (attrs) {
        return "(" + attrs + ") => { return {_tag: '" + id + "', " + attrs + "} }"
      }
    }
    return "{_tag: '" + id + "'}"
  }
  function parse_class() {
    return reg(/^(\w+)(?: \w+)*? class:/).and(x =>
      many1(reg(/^\n  (\w+) ([^=\n]+)(?=\n)/)).and(vars =>
        many(read_indent(1).and(parse_line(1, def_mode))).and(methods =>
          to_class(x[1], vars, methods))))
  }
  function to_class(id, vars, methods) {
    const args = vars.map(x => x[1])
    const names = methods.map(x => x.match(/const (\w+)/)).filter(x => x).map(x => x[1])
    const ids = args.concat(names).join(", ")
    return "const " + id + " = (" + args.join(", ") + ") => {\n  "+
      methods.join("\n") +
      "\nreturn {_tag: '" + id +"', " + ids + " }" +
    "\n}"
  }
  // converter
  function fix_exp(input) {
    return split_seq([input]).map(fix_one_exp).join("; ")
  }

  function fix_one_exp(input) {
    input = input.trim()
    let n = 0
    const exp = input.replace(/\(\w+(?: \w+)* = .+/m, to_func).replace(/^[a-zA-Z]\w*\(.+?\)$/, part =>
      part.replace(/(?<=[\w"')\]]) (?=[\w"'(\[])/g, ", ")
    ).replace(/[a-zA-Z]\w*\(.+?(?<!\()\)/g, part =>
      part.replace(/(?<=[\w"')\]]) (?=[\w"'(\[])/g, ", ")
    ).replace(/\[\S+( \S+)+\]/g, part =>
      part.replace(/(?<=[\w"')]) (?=[\w"'(])/g, ", ")
    ).replace(/(?<!\w)\([^,)]+(?:, [^,)]+)+\)/g, part =>
      part.replace("(", "({n0:").replace(")", "})").replace(/, /g, _ =>
        ", n" + ++n + ":"
      )
    ).replace(/ := /g, " = ").replace(/\.(\d+)/g, ".n$1")
    const lr = exp.split(" | ", 2)
    if (lr.length == 2) {
      return "(() => { try { return " + lr[0] + "} catch(e) { if(e.isMoa) { return " + lr[1] + "} else { throw(e) } } })()"
    }
    return exp
  }
  function to_func(s) {
    const m = s.match(/\((\w+)((?: \w+)*) = (.+)\)\(([^)]*)\)/)
    const id = m[1]
    const args = m[2].trim().replace(/ /g, ",")
    const body = fix_exp(m[3])
    const argv = m[4].trim().replace(/ /g, ",")
    const func = "(function " + id + "(" + args + ") { return " + body + " })(" + argv + ")"
    return func
  }
  function split_seq(lines) {
    return lines.reduce((acc, x) => acc.concat(x.split(";")), [])
  }
  function to_block(exp) {
    const seq = split_seq(Array.isArray(exp) ? exp : [exp])
    if (seq.length === 1) {
      return seq[0]
    } else {
      const last = seq[seq.length - 1]
      const m = last.match(/^const (\w+)/)
      if (m) {
        seq[seq.length - 1] = last + "; return " + m[1]
      } else {
        seq[seq.length - 1] = "return " + last
      }
      return "(() => { " + seq.join("; ") + " })()"
    }
  }
  function to_match(x, y) {
    if (y === "_") {
      return "true"
    } else if (y.match(/^[a-zA-Z][\w.]*$/)) {
      return x + "._tag === '" + y + "'"
    } else {
      return x + " === " + y
    }
  }
  // helpers
  function read_indent(offset) {
    return reg("\n+" + "  ".repeat(depth + offset))
  }
  function read_fork_match(exp) {
    return many1(read_indent().and(reg(/^\| ([^=]+) = /)).and(cond =>
      parse_exp().and(x => "if (" + to_match(exp, cond[1]) + ") { return " + x + "}"))).and(xs => "(() => {" + xs.join("\n") + "\nreturn null })()")
  }
  function read_fork_bool(exp) {
    return read_indent().and(reg(/\| /)).and(parse_exp).and(t =>
      read_indent().and(reg(/\| /)).and(parse_exp).and(f =>
          exp + " ? " + t + " : " + f))
  }
  // combinators
  function many(p, acc) {
    return p.and(x => many(p, append(acc, x))).or(() => acc || [])
  }
  function many1(p, acc) {
    return p.and(x => many(p, [x]))
  }
  function sep_by1(p, s) {
    return p.and(x => many(s.and(p), [x]))
  }
  function first(xs) {
    let x = xs[0]
    for (let i = 1; i<xs.length; ++i) {
      x = x.or(xs[i])
    }
    return x
  }
  // matcher
  function reg(x) {
    return promise(() => {
      const m = src.slice(pos).match(x)
      if (m === null || m.index !== 0) {
        return failure
      }
      pos += m[0].length
      return m
    })
  }
  // promise
  const failure = "'--parse-failed--'"
  function promise(run) {
    return {
      run,
      isPromise: true,
      and: f => promise(() => flow(run, f, fail)),
      or: f => promise(() => flow(run, echo, f)),
    }
  }
  function flow(f0, f1, f2) {
    let memory = pos
    let x = unwrap(f0)
    if (x === failure) {
      pos = memory
      return unwrap(f2)
    } else {
      return unwrap(f1, x)
    }
  }
  function unwrap(x, argv) {
    const t = typeof(x)
    if (t === 'object'  && x.isPromise) {
      return unwrap(x.run())
    } else if (t === 'function') {
      return unwrap(x(argv))
    } else {
      return x
    }
  }
  function fail() { return failure }
  function echo(x) { return x }

  // run parser
  const ret = unwrap(parse_top())
  if (ret === failure || pos !== src.length) {
    warn("parse failed" +
      "\n|    pos: " + pos +
      "\n| remain: " + JSON.stringify(src.slice(pos)))
  }
  return ret
}

function prepare() {
  this.error = x => {
    const e = new Error("error: " + x)
    e.isMoa = true
    throw(e)
  }
  function guard(cond, ret, msg) { if (cond) { return ret } else { error(msg) } }
  Array.prototype.head = function() { return guard(this.length > 0, this[0], "out of index") }
  Array.prototype.tail = function() { return this.slice(1) }
  Array.prototype.n0 = function() { return this[0] }
  Array.prototype.n1 = function() { return this[1] }
  Array.prototype.n2 = function() { return this[2] }
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
  String.prototype.to_i = function() { return parseInt(this) }
  String.prototype.to_a = function() { return this.split("") }
  Number.prototype.to_s = function() { return String(this) }
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
      log("ok: " + fact)
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
  t(6, "add a b = a + b\nadd(1 2 + 3)")
  // exp(8)
  t(3, "1 + 2")
  //t(4, "9 / 2")
  t(1, "a = 1\na()")
  t(2, "a 1\nincr = a += 1\nincr()\na")
  t(1, "(x => x)(1)")
  t(1, "true\n| 1\n| 2")
  t(2, "false\n| 1\n| 2")
  t(true, "1\n| 1 = true\n| 2 = false")
  t(false, "3\n| 1 = true\n| _ = false")
  t(1, "ab enum:\n  a\n  b\nab.a\n| a = 1\n| b = 2")
  t(2, "ab enum:\n  a\n  b\nab.b\n| a = 1\n| b = 2")
  // container(5)
  t([1], "[1]")
  t([1, 5], "[1 2 + 3]")
  t(5, "(1, 2 + 3).1")
  t(3, "ab enum:\n  a x int\n  b y int\nab.a(3).x")
  t(1, "s class:\n  n int\n  m int\ns(1 2).n")
  // error(2)
  t("error: failed", "f x = error(\"failed\")\nf(1)")
  t(2, "error(\"failed\") | 2")
  // built-in
  t(1, "\"01\".to_i()")
  t("1,2,3", "[1 2 3].map(x=>x.to_s()).join(\",\")")
  t(1, "[1].nth(0)")
  t(5, "[1 2 + 3].nth(1)")
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
