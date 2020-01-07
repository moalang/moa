const log = console.log
const warn = console.error
function debug(x) {
  log("| " + JSON.stringify(x))
}
function append(x, y) {
  let z = x || []
  z.push(y)
  return z
}
function parse(src) {
  let pos = 0
  let depth = 0

  // parser
  function parse_top() {
    return parse_lines()
  }
  function parse_lines() {
    return sep_by1(parse_line(), read_indent(0)).and(x => x.join(";\n"))
  }
  function parse_line(offset) {
    return parse_enum().or(parse_class).or(parse_var).or(parse_func(offset)).or(parse_fork)
  }
  function parse_var() {
    return reg(/^(\w+) ([^=: ]+)$/m).and(m => {
      return "let " + m[1] + " = " + m[2]
    })
  }
  function parse_func(offset) {
    return reg(/^( *)(\w+)((?: \w+)+)? *= */m).and(m => {
      const id = m[2]
      const args = (m[3] || "").trim().split(" ").join(", ")
      return parse_body(offset).and(exp => "const " + id + " = (" + args + ") => " + exp)
    })
  }
  function parse_body(offset) {
    offset = offset || 0
    return many1(read_indent(1 + offset).and(parse_line())).and(to_block).or(parse_line())
  }
  function parse_fork() {
    return parse_exp().and(x =>
      read_fork_match(x).or(read_fork_bool(x)).or(x))
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
      enums.map(x => to_enum_tag(x[1], x[2])).join(",\n") +
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
        many(read_indent(1).and(parse_line(1))).and(methods =>
          to_class(x[1], vars, methods))))
  }
  function to_class(id, vars, methods) {
    const args = vars.map(x => x[1])
    const ids = args.concat(methods.map(x => x.match(/const (\w+)/)[1])).join(", ")
    return "const " + id + " = (" + args.join(", ") + ") => {\n  "+
      methods.join(";\n  ") +
      "\n  return {_tag: '" + id +"', " + ids + " }" +
    "\n}"
  }
  // converter
  function fix_exp(line) {
    let n = 0
    line = line.replace(/[a-zA-Z]\w*\(.+?\)/g, part =>
      part.replace(/(?<=[\w"']) (?=[\w"'])/g, ", ")
    ).replace(/\[\S+( \S+)+\]/g, part =>
      part.replace(/(?<=[\w"']) (?=[\w"'])/g, ", ")
    ).replace(/(?<!\w)\([^,)]+(?:, [^,)]+)+\)/g, part =>
      part.replace("(", "({n0:").replace(")", "})").replace(/, /g, _ =>
        ", n" + ++n + ":"
      )
    ).replace(/ := /g, " = ")
    const lr = line.split(" | ", 2)
    if (lr.length == 2) {
      line = "(() => { try { return " + lr[0] + "} catch(e) { if(e.isMoa) { return " + lr[1] + "} else { throw(e) } } })()"
    }
    return line
  }
  function to_block(lines) {
    if (lines.length === 1) {
      return lines[0]
    } else {
      const last = lines[lines.length - 1]
      const m = last.match(/^const (\w+)/)
      if (m) {
        lines[lines.length - 1] = last + ";\nreturn " + m[1]
      } else {
        lines[lines.length - 1] = "return " + last
      }
      return "(() => {\n" + lines.join(";\n") + "\n})()"
    }
  }
  function to_match(x, y) {
    if (y === "_") {
      return "true"
    } else if (y.match(/^[a-zA-Z]\w*$/)) {
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
      parse_exp().and(x => "if (" + to_match(exp, cond[1]) + ") { return " + x + "}"))).and(xs => to_block(append(xs, "null")))
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
  Array.prototype.n1 = function(n) { return this[n] }
  Array.prototype.nth = function(n) { return this[n] }
  Array.prototype.contains = function(x) { return this.indexOf(x) !== -1 }
  String.prototype.nth = function(n) { return this[n] }
  String.prototype.to_i = function() { return parseInt(this) }
  String.prototype.to_a = function() { return this.split("") }
  Number.prototype.to_s = function() { return String(this) }
  this.error = x => {
    const e = new Error("error: " + x)
    e.isMoa = true
    throw(e)
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
  t(2, "inc a = a + 1\ninc(1)")
  t(6, "add a b = a + b\nadd(1 2 + 3)")
  // exp(8)
  t(3, "1 + 2")
  t(-1, "1 - 2")
  t(6, "2 * 3")
  //t(4, "9 / 2")
  t(1, "a = 1\na()")
  t(3, "c = 1\nb n = n + c()\na = b(2)\na()")
  t(2, "a 1\nincr = a += 1\nincr()\na")
  t(2, "a =\n  1\n  2\nb = a(); a()\nc = b()\nc()")
  t(1, "true\n| 1\n| 2")
  t(2, "false\n| 1\n| 2")
  t(true, "1\n| 1 = true\n| 2 = false")
  t(false, "2\n| 1 = true\n| 2 = false")
  t(false, "3\n| 1 = true\n| _ = false")
  t(1, "ab enum:\n  a\n  b\nab.a\n| a = 1\n| b = 2")
  t(2, "ab enum:\n  a\n  b\nab.b\n| a = 1\n| b = 2")
  // container(5)
  t([1], "[1]")
  t([1, 5], "[1 2 + 3]")
  t(5, "(1, 2 + 3).n1")
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
  t(5, "(1, 2 + 3).n1")
  log("---( complex pattern )---------")
  // container(5)
  t(4, "ab enum:\n  a x int\n  b y int, z int\nab.b(1).y + ab.b(2 3).z")
  t(9, "s class:\n  n int\n  incr = n += 1\n  incr2 = incr()\n  mul x =\n    n := n * x\nt s(1)\nt.incr()\nt.incr2()\nt.mul(3)")
  t(10, "s class:\n  n int\n  f1 =\n    n\n  f2 =\n    f1()\ns(10).f2()")
  // error(2)
  t("message", "f x = error(x)\ncalc =\n  r y = f(y) | y\n  r(\"message\")\ncalc()")
  log("done")
}

if (process.argv[2] === "test") {
  prepare()
  test()
} else {
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
  log(code)
}
