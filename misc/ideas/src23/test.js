# let
assert 3:
  let n 1 + 2
  n

# var
assert 3:
  var a 1
  a += 2
  a
assert 1:
  var a []
  a.push 1
  a.size
assert 1:
  var a dict()
  a[1] := 2
  a.size

# function
assert 1:
  def f 1
  f
assert 1:
  def f a: a
  f 1
assert 3:
  def a b: a + b
  f 1 2
assert 3:
  var a 1
  def f b: a += b
  f(2)
  a
assert 6:
  def f a b:
    if a.size == 0:
      b
      f a.slice(1) a[0] + b
  f [1 2 3] 0

# record
assert 3:
  record s: a int; b int
  s(1 2).a + s(1 2).b
# bool
assert true !false
assert true  true  || true
assert true  true  && true

# number
assert 3 1 +  2
assert 1 3 -  2
assert 6 2 *  3
assert 2 4 /  2
assert 1 5 %  2
assert 3 1 |  2
assert 2 3 &  2
assert 1 3 ^  2
assert 8 2 ** 3
assert true  1 == 1
assert false 1 != 1
assert false 1 >  1
assert true  1 >= 1
assert false 1 <  1
assert true  1 <= 1
assert 2: var a 1; a +=  1; a
assert 1: var a 3; a -=  2; a
assert 6: var a 2; a *=  3; a
assert 2: var a 4; a /=  2; a
assert 1: var a 5; a %=  2; a
assert 3: var a 1; a |=  2; a
assert 2: var a 3; a &=  2; a
assert 1: var a 3; a ^=  2; a
assert 8: var a 2; a **= 3; a

# lambda
assert 1 fn(1)()
assert 1 fn(a a)(1)

# string
assert "ab" "a" ++ "b"

# list
assert [1 2] [1] ++ [2]
assert 1 [1][0]
assert [2]: var a [1]; a[0] = 2; a

# struct
assert 1 struct(a 1).a
assert 2 struct(a 1 b 2).b

# set
assert set(1)   set(1 2) - set(2)
assert set(1 2) set(1)   | set(2)
assert set(2)   set(1 2) & set(2)
assert set(1)   set(1 2) ^ set(2)

# dict
assert dict(1 2 3 4) dict(1 2) ++ dict(3 4)

# if
assert 1: if true 1 [][0]
assert 2: if false [][0] 2
assert 2: if false [][0] true 2 [][0]
assert 3: if false [][0] false [][0] 3

# case
assert "a": case 1 1 "a" 2 "b" _ "c"
assert "b": case 2 1 "a" 2 "b" _ "c"
assert "c": case 3 1 "a" 2 "b" _ "c"

# short circuit evaluation
assert false false && [][0]
assert true  true || [][0]

# do
assert 2 (1; 2)

# throw / catch
assert "a" catch(throw("a") fn(e e.message))
# shell
assert ";\n" io.shell("echo" "';'").result
assert 1 catch(io.shell("nocommand").result fn(e 1))

# fs
assert "Hello\n":
  io.fs.write("hello.txt" "Hello\n")
  let a io.fs.reads("hello.txt")
  io.fs.rm("hello.txt")
  a
# number
assert 1 (-1).abs
assert 1 (-1).neg
assert (-1) 1.neg
assert "A" 65.char
assert 1 1.9.floor
assert 2 1.1.ceil
assert 1 1.4.round
assert 2 1.5.round

# string
assert 2       "ab".size
assert "ba"    "ab".reverse
assert "ba"    "aba".slice(1)
assert "b"     "aba".slice(1 2)
assert "_b_"   "aba".replace("a" "_")
assert some(1) "ab".index("b")
assert none    "ab".index("c")
assert true    "ab".starts("a")
assert false   "ab".starts("b")
assert true    "\\"".starts("\\"")
assert false   "ab".ends("a")
assert true    "ab".ends("b")
assert "ab"    "\t\r\n ab \t\r\n".trim

# regexp
assert true          regexp("^\\d+$").match("123")
assert false         regexp("^\\d+$").match("1.2")
assert ["1" "2"] regexp("\\d").capture("1.2")
assert []        regexp("a").capture("1.2")
assert ["a" "b"] regexp("\\d").split("a1b")
assert "1!2!"        regexp("\\d").replace("12" fn(x x ++ "!"))

# time
assert 2024 time(2024  1 2 3 4 5 6).year
assert    1 time(2024  1 2 3 4 5 6).month
assert    2 time(2024  1 2 3 4 5 6).day
assert    3 time(2024  1 2 3 4 5 6).hour
assert    4 time(2024  1 2 3 4 5 6).min
assert    5 time(2024  1 2 3 4 5 6).sec
assert    6 time(2024  1 2 3 4 5 6).offset
assert    2 time(2024  1 2 3 4 5 6).wday
assert  337 time(2024 12 2 3 4 5 6).yday
assert "2024-01-02T03:04:05Z"     time(2024 1 2 3 4 5 0).string
assert "2024-01-02T04:05:06Z"     time(2024 1 2 3 4 5 0).tick(3661).string
assert "2024/1/2 3:4:5 -0006"     time(2024 1 2 3 4 5 (-6)).format("yyyy/m/d H:M:S z")
assert "2024/01/02 03:04:05+0006" time(2024 1 2 3 4 5 6).format("yyyy/mm/dd HH:MM:SSz")
assert "2024/01/02 03:03:59+0000" time(2024 1 2 3 4 5 6).utc.format("yyyy/mm/dd HH:MM:SSz")

# option
assert some(3) some(1).and(fn(a (+ a 2)))
assert 1 some(1).or(2)
assert 2 none.or(2)
assert true some(0).bool
assert false none.bool

# tuple
assert 1   tuple(1 "a").0
assert "a" tuple(1 "a").1

# list
assert 1 [1].at(0)
assert 1 [1].at((-1))
assert 2 [1 2].at((-1))
assert 2 [1].tie(0 2)
assert [2]: var a [1]; a.tie 0 2; a
assert [1]: var a []; a.push 1; a
assert some(1)          [1].get(0)
assert none             [1].get(1)
assert 1                [1].size
assert [2 1]            [1 2].reverse
assert [2]              [1 2].slice(1)
assert [1]              [1 2].slice(0 1)
assert [2]              [1].map(fn(a a + 1))
assert [1 3]            [1 2].mapi(fn(a i a + i))
assert [1 2]            [1].fmap(fn(a [a] a + 1))
assert [1]              [1 2].keep(fn(a a == 1))
assert false            [1 2].all(fn(a a < 2))
assert true             [1 2].any(fn(a a < 2))
assert [1 2]            [2 1].sort()
assert [1 2]            [1 2].sort(fn(a b a > b))
assert [tuple(1 2)]     [1].zip([2])
assert 3                [1 2].fold(0 fn(a b (+ a b)))
assert some(1)          [1].find(fn(a a == 1))
assert none             [1].find(fn(a a == 2))
assert some(0)          [1].index(fn(a a == 1))
assert none             [1].index(fn(a a == 2))
assert "a,b"            ["a" "b"].join(",")
assert true             [1].has(1)
assert 1                [1 2].min
assert 2                [1 2].max

# set
assert 1     set(1).size
assert true  set(1).has(1)
assert false set(1).add(1)
assert true  set(1).add(2)
assert true  set(1).rid(1)
assert false set(1).rid(2)

# dict
assert some(2)      dict(1 2).get(1)
assert none         dict(1 2).get(2)
assert 1            dict(1 2).size
assert true         dict(1 2).has(1)
assert 2            dict().set(1 2)
assert [1]          dict(1 2).keys
assert [2]          dict(1 2).values
assert [tuple(1 2)] dict(1 2).list
