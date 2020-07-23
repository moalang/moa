const moa = require("./moa.js")
const log = x => console.log(x)
const trace = x => console.warn({trace: x})
const warn = x => console.warn({warn: x})
const debug = x => warn(JSON.stringify(x))
function run(js) {
  try {
    return eval(js)
  } catch (e) {
    if (!e.isMoa) {
      warn("failed to eval: " + e.message + "\n" + e.stack + js)
    }
    return e.message
  }
}

function test_main() {
  function t(expect, main) {
    const defs = Array.from(arguments).slice(2)
    const src = "main = " + main + (defs || []).map(x => "\n" + x).join("")
    const js = moa.parse(src)
    const fact = run(js + "\n" + "__moa__.call(main)")
    if (JSON.stringify(expect) === JSON.stringify(fact)) {
      log("ok: " + JSON.stringify(fact))
    } else {
      log("expect: " + JSON.stringify(expect))
      log("  fact: " + JSON.stringify(fact))
      log("    js: " + js.split("\n").join("\n      | "))
      log("   moa: " + src.split("\n").join("\n      | "))
    }
  }

  moa.prepare(this)
  test_values(t)
  test_io(t)
  log("done")
}

function test_io(t) {
  const fs = require("fs");
  const { execSync } = require('child_process')

  function spawn(expect, exp, input) {
    input = input || ""
    const src = "- io\nexp = " + exp + "\nmain = io.write(exp)"
    const js = moa.compile(src)
    fs.writeFileSync('/tmp/moa.js', js);
    fs.writeFileSync('/tmp/moa.txt', input);
    const fact = execSync('node /tmp/moa.js < /tmp/moa.txt').toString()
    if (expect === fact) {
      log("ok: " + JSON.stringify(fact))
    } else {
      log("expect: " + JSON.stringify(expect))
      log("  fact: " + JSON.stringify(fact))
      log("    js: " + js.split("\n").join("\n      | "))
      log("   moa: " + src.split("\n").join("\n      | "))
    }
  }
  log("---( io )---------")
  spawn("hi", "io.stdin.read", "hi")
}

function test_values(t) {
  log("---( basic values )---------")
  // value(4)
  t(1, "1")
  t("hello world", "\"hello world\"")
  t(true, "true")
  t(false, "false")
  t(true, "1 == 1")
  t(1, "inc(0)", "inc a = a + 1")
  t(2, "id(2)", "id = x => x")
  t(3, "add(1 2)", "add a b = a + b")
  log("-- exp(8)")
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
  t(9, "{ e <- f\n0 } ||| 9", "f = { error(\"fail\") }")
  log("-- container(5)")
  t([1], "[1]")
  t([1, 2], "[1 2]")
  t([1, 2, 3], "[1 2 3]")
  t(2, "(1, 2).1")
  t(3, "ab.a(3).x", "ab:|\n  a x int\n  b y int")
  t(4, "s(4 0).n", "s:\n  n int\n  m int")
  log("-- error(2)")
  t("error: failed", "f(1)", "f x = error(\"failed\")")
  t(2, "error(\"failed\") ||| 2")
  log(" built-in")
  t(1, "\"01\".to_i")
  t("1,2,3", "[1 2 3].map(x => x.to_s).join(\",\")")
  t(1, "[1].nth(0)")
  t([1, 5], "[1 (2 + 3)]")
  t("i", "\"hi\".nth(1)")
  t(5, "(1, (2 + 3)).1")

  log("---( complex values )---------")
  log("-- value(4)")
  t('\"', '"\\""')
  t(1, "call(() => 1)", "call f = f()")
  t(2, 'call("\\"" "\\"")', "call a b = 2")
  log("-- exp(8)")
  t(3, "a", "c = 1\nb n = n + c\na = b(2)")
  t(4, "c + 2", "a =\n  1\n  2\nb =\n  a\n  a\nc = b")
  t(5, "f + 4", "f =\n  v = 1\n  v")
  t(6, "5 + g(1 \"a\" f(2) 3)", "f x = x\ng a b c d = a")
  t(7, "1 + sum([1 2 3])", "sum xs = (f acc xs = f(acc + xs.head xs.tail) ||| acc)(0 xs)")
  t(1, "f(1)", "f x = x\n| 1 = 1\n| _ = 2")
  t(8, "g(f(8) 2 g(3) f(4))", "f x = x\ng a b c d = a")
  log("-- container(5)")
  t(5, "(1, (2 + 3)).1")
  t([1, 5], "[1 (2 + 3)]")
  t(4, "ab.b(1).y + ab.b(2 3).z", "ab:|\n  a x int\n  b y int, z int")
  t(3, "f(ab.a) + f(ab.b)", "ab:|\n  a\n  b\nf x = x\n| a = 1\n| b = 2")
  //t(9, "s(8).incr",  "s:\n  n int\n  incr { n += 1 }")
  t(9, "s(8).incr",  "s:\n  n int\n  incr = { n += 1 }")
  t(10, "s(10).f2", "s:\n  n int\n  f1 =\n    n\n  f2 =\n    f1")
  t(11, "10 + s(1).f", "s:\n  n int\n  f =\n    v = n\n    v")
  log("-- error(2)")
  t("message", "calc", "f x = error(x)\ncalc =\n  r y = f(y) ||| y\n  r(\"message\")")
  log("-- indent")
  t(1, "p.n", "p:\n  m =\n    n\n  n =\n    1")
  t(2, "or(1 2)", "or l r =\n  a = l\n  alt x = 1; x\n  2 ||| alt(1)")
}

if(require.main === module) {
  test_main()
}
