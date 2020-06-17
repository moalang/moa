const str = o => JSON.stringify(o)
const put = s => process.stdout.write(s)
const puts = (...args) => console.log(...args)

function parse(source) {
  let remaining = source
  const miss = (s, o) => {
    let e = new Error("miss mtach " + s + " remaining=" + remaining + " " + str(o) + " source=" + source)
    e.miss = true
    return e
  }
  const call = (f, ...args) => typeof f === "function" ? f(...args) : f
  const reg = r => {
    const m = remaining.match(r)
    if (m) {
      remaining = remaining.slice(m[0].length)
      return m[1]
    } else {
      throw miss("reg", r.source)
    }
  }
  const eq = s => {
    if (remaining.startsWith(s)) {
      remaining = remaining.slice(s.length)
      return s
    } else {
      throw miss("eq", s)
    }
  }
  const seq = (...sequence) => {
    let ret
    sequence.forEach(s => { ret = call(s, ret) })
    return ret
  }
  const take = s => { reg(/^( *)/); return eq(s); }
  const or = (...cases) => {
    for (let i=0; i<cases.length; ++i) {
      try {
        return call(cases[i])
      } catch (e) {
        if (!e.miss) {
          throw e
        }
      }
    }
    throw miss("or", cases)
  }
  const between = (l, r, m) => seq(
      take(l),
      m,
      ret => { take(r); return ret }
  )
  const many = f => {
    let result = []
    const go = () => { result.push(f()); go() }
    return or(go, () => result)
  }
  const sepby = (f, s) => {
    let result = []
    try {
      result.push(f())
      const go = () => { take(s); result.push(f()); go() }
      return or(go, () => result)
    } catch (e) {
      if (!e.miss) {
        throw e
      }
      return []
    }
  }
  const make_struct = xs => {
    let s = {}
    for(let i=0; i<xs.length; ++i) {
      s[xs[i][0]] = xs[i][1]
    }
    return s
  }
  const read_op = () => reg(/^ *([\+\-\*\/])/)
  const read_op2 = (op, l) => build(l) + " " + op + " " + build(parse_value())
  const read_kv = () => {
    const id = reg(/^ *(\w+)/)
    const type = parse_value()
    return [id, type]
  }
  const parse_top = () => {
    if (remaining.match(/^ *\)/)) {
      return {}
    } else if (remaining.match(/^ *(\w)+ ([\w\(\["])+/)) {
      return make_struct(sepby(read_kv, ","))
    } else {
      const v = sepby(parse_value, ",")
      return v.length === 1 ? v[0] : v
    }
  }
  const parse_value = () => or(
    () => parseInt(reg(/^ *(\d+(?:\.\d+)?)/)),
    () => reg(/^ *("[^"]*")/),
    () => reg(/^ *(true|false)/) == "true",
    () => between("[", "]", () => many(parse_value)),
    () => between("(", ")", parse_top))
  const parse_exp = () => {
    const l = parse_value()
    return or(() => seq(read_op, op => read_op2(op, l)), l)
  }
  const build = o => o
  return parse_exp()
}

function run(source) {
  const js = parse(source)
  return eval(js)
}

function run_test() {
  let errors = []
  function eq(expect, source) {
    const actual = run(source)
    if (str(expect) === str(actual)) {
      put(".")
    } else {
      put("x")
      errors.push({expect, actual, source})
    }
  }

  // value
  eq(1, "1")
  eq(1.0, "1.0")
  eq("hi", "\"hi\"")
  eq(true, "true")
  // container
  eq([1, 2, 3], "[1 2 3]")
  eq([1, 2], "(1, 2)")
  eq({x: 1, y: 2}, "(x 1, y 2)")
  //eq({1: true, 2: false}, "{1 true, 2 false}")
  // exp
  eq(5, "2 + 3")
  eq(-1, "2 - 3")
  eq(6, "2 * 3")
  eq(2, "6 / 3")
/*
  eq("2", "inc a = a + 1\ninc(1)")
  eq("6", "add a b = a + b\nadd(1 2 + 3)")

  -- value(4)
  test "1" "1"
  test "hello world" "\"hello world\""
  test "true" "true"
  test "false" "false"
  test "2" "inc a = a + 1\ninc(1)"
  test "6" "add a b = a + b\nadd(1 2 + 3)"
  -- exp(8)
  test "3" "1 + 2"
  test "-1" "1 - 2"
  test "6" "2 * 3"
  test "4" "9 / 2"
  test "1" "a = 1\na"
  test "3" "c = 1\nb n = n + c\na = b(2)\na"
  test "2" "a = 1\nincr = a += 1\nincr\na"
  test "2" "a =\n  1\n  2\nb = a; a\nc = b\nc"
  test "1" "true\n| 1\n| 2"
  test "2" "false\n| 1\n| 2"
  test "true" "1\n| 1 = true\n| 2 = false"
  test "false" "2\n| 1 = true\n| 2 = false"
  test "false" "3\n| 1 = true\n| _ = false"
  test "1" "ab enum:\n  a\n  b\nab.a\n| a = 1\n| b = 2"
  test "2" "ab enum:\n  a\n  b\nab.b\n| a = 1\n| b = 2"
  -- container(5)
  test "1" "[1 2](0)"
  test "5" "[1 2+3](1)"
  test "5" "[1, 2+3](1)"
  test "5" "[1, 2+3].n1"
  test "1" "s class: n int, m int\ns(1 2).n"
  test "3" "ab enum:\n  a x int\n  b y int\nab.a(3).x"
  test "4" "ab enum:\n  a x int\n  b y int\nab.b(4).y"
  -- error(2)
  test "error: divide by zero" "1 / 0"
  test "2" "1 / 0 | 2"
  -- built-in
  test "1" "\"01\".to_i"
  test "1,2,3" "[1 2 3].map(x -> x.to_s).join(\",\")"
  -- bugs
  test "1" "id x = x\nid1 x = id(x)\nid1(1)"
  putStrLn "done"
*/

  puts(errors.length === 0 ? "ok" : "FAILED")
  errors.forEach(e => {
    puts("source: " + e.source)
    puts("expect: " + str(e.expect))
    puts("actual: " + str(e.actual))
  })
  return errors.length
}

function main(command) {
  if (command === "test") {
    process.exit(run_test())
  } else {
    var input = require('fs').readFileSync('/dev/stdin', 'utf8');
    puts(input)
  }
}

main(process.argv[2])
