const puts = console.log
const error = console.error
function debug(x) {
  puts("| " + JSON.stringify(x)) 
}
function parse(src) {
  let pos = 0
  let depth = 0

  // parser
  function parse_top() {
    return parse_lines()
  }
  function parse_lines() {
    return sep_by1(parse_line(), read_indent(0)).and(x => x.join("\n"))
  }
  function parse_line() {
    return parse_func().or(parse_exp)
  }
  function parse_func() {
    return reg(/^( *)(\w+)((?: \w+)+)? *=/m).and(m => {
      depth = m[1].length / 2
      const id = m[2]
      const args = (m[3] || "").trim().split(" ").join(", ")
      if (args === "") {
        return parse_body().and(exp => "let " + id + " = " + exp)
      } else {
        return parse_body().and(exp => "const " + id + " = (" + args + ") => " + exp)
      }
    })
  }
  function parse_body() {
    return many1(read_indent(1).and(parse_line())).and(to_block).or(parse_exp())
  }
  function parse_exp() {
    return reg(/[^\n]+/).and(x => to_apply(x[0]))
  }
  // converter
  function to_apply(line) {
    return line.replace(/[a-zA-Z]\w+\(.+?\)/g, part => {
      return part.replace(/ /g, ", ")
    })
  }
  function to_block(lines) {
    if (lines.length === 1) {
      return lines[0]
    } else {
      lines[lines.length - 1] = "return " + lines[lines.length - 1]
      return "(() => {\n" + lines.join("\n") + "\n})()"
    }
  }
  // helpers
  function read_indent(offset) {
    return reg("\n" + "  ".repeat(depth + offset))
  }
  // combinators
  function many(p, acc) {
    return p.and(x => many(p, (acc || []).concat(x))).or(() => acc)
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
    console.error("parse failed" +
      "\n|    pos: " + pos +
      "\n| remain: " + JSON.stringify(src.slice(pos)))
  }
  return ret
}

function run(js) {
  try {
    return eval(js)
  } catch (e) {
    console.error("failed to eval: " + JSON.stringify(js))
    return e.message
  }
}

function test() {
  function t(expect, src) {
    const js = parse(src)
    const fact = run(js)
    if (expect === fact) {
      console.log("ok: " + fact)
    } else {
      console.log("expect: " + JSON.stringify(expect))
      console.log("  fact: " + JSON.stringify(fact))
      console.log("    js: " + JSON.stringify(js))
    }
  }
  // value(4)
  t(1, "1")
  t("hello world", "\"hello world\"")
  t(true, "true")
  t(false, "false")
  t(2, "inc a = a + 1\ninc(1)")
  t(6, "add a b = a + b\nadd(1 2+3)")
  //// exp(8)
  t(3, "1 + 2")
  t(-1, "1 - 2")
  t(6, "2 * 3")
  //t(4, "9 / 2")
  t(1, "a = 1\na")
  t(3, "c = 1\nb n = n + c\na = b(2)\na")
  t(2, "a = 1\nincr = a += 1\nincr\na")
  t(2, "a =\n  1\n  2\nb = a; a\nc = b\nc")
  t(1, "true\n| 1\n| 2")
  //t("2", "false\n| 1\n| 2")
  //t("true", "1\n| 1 = true\n| 2 = false")
  //t("false", "2\n| 1 = true\n| 2 = false")
  //t("false", "3\n| 1 = true\n| _ = false")
  //t("1", "ab enum:\n  a\n  b\nab.a\n| a = 1\n| b = 2")
  //t("2", "ab enum:\n  a\n  b\nab.b\n| a = 1\n| b = 2")
  //// container(5)
  //t("1", "[1 2](0)")
  //t("5", "[1 2+3](1)")
  //t("5", "[1, 2+3](1)")
  //t("5", "[1, 2+3].n1")
  //t("1", "s class: n int, m int\ns(1 2).n")
  //t("3", "ab enum:\n  a x int\n  b y int\nab.a(3).x")
  //t("4", "ab enum:\n  a x int\n  b y int\nab.b(4).y")
  //// error(2)
  //t("error: divide by zero", "1 / 0")
  //t("2", "1 / 0 | 2")
  //// built-in
  //t("1", "\"01\".to_i")
  //t("1,2,3", "[1 2 3].map(x -> x.to_s).join(\",\")")
  //// bugs
  //t("1", "id x = x\nid1 x = id(x)\nid1(1)")
  puts("done")
}

test()
