# The Moa Programming Language
Moa is an open source programming language.
This language maximize productivity by fun programming.

Table of contents
0. Hello world
1. Value
2. Expression
3. Definition
4. Namespace
5. Buildin
6. Appendix
7. TODOs





## 0. Hello world

Set up
```
mkdir -p ~/moa/bin
curl https://github.com/moa/releases/mac/latest > ~/moa/bin/moa
export PATH=$PATH:~/moa/bin
```

Make a project
```
# moa new
├test
│ └test.moa
└src
   └main.moa
```

Run
```
> moa run
Hello World.

# main.moa
main = io.print("Hello World")
```

Test
```
> moa test
..x. Failed
test.moa:5|  eq(12 v)
expect: 12
  fact: 11

# test.moa
- test
main =
  v = 11
  test.eq(1 1)
  test.eq(12 v)
  test.eq(1 1)
```





## 1. Value

Primitive
```
true # bool
1    # int 64bit signed integer
1.0  # real 64bit
"hi" # string utf8
```

Container
```
(1 2)
(x:1 y:2)
array(1 2)
dict(1 2 k v)
dict(x:1 y:2)
set(1 2)
counter(1 2)
```

Closure
```
a,b => a + b # closure
```





## 2. Expression

Binary operation
```
1 + 2 * 3 # = 7
```

Branch
```
a > b
| "true"
| "false"

n
| 0 = "zero"
| 1 = "one"
| _ = "many"

statement
| v -> "succeed $v"
| e -> "failed $e"
```

Effect
```
value <- calculate(1)
func
```





## 3. Definition

Function
```
pi: float
pi = 3.14

add: int int int
add x y = x + y

echo =
  line <- io.readline
  io.print(line)
```

Generics
```
id a: a a
id x = x

sum a.num: []a a
sum t => xs = xs.reduce((+) t.zero)
```

Default value
```
join: []string string=" " string="" string
join ary glue alt = ary
| [] = alt
| _ = ary.join(glue)
```

Variable arguments
```
trace: any+ io
trace argv+ = print(argv.map(x => x.string).join(" "))

printf: fmt args* io
printf fmt args* = print(fmt.format(args))
```

Named arguments
```
copy from:string to:string io
copy from to = io.fs.copy(from to)
# error   <= copy("" "")
# ok      <= copy(from:"a" to:"b")
# ok      <= copy(to:"a" from:"b")
```

Named optional arguments
```
estimate: decimal tax:decimal service:decimal=0 decimal
estimate: price tax service = (price + service) * (1 + tax)
# error <= estimate(109)
# error <= estimate(109 0.1)
# 110   <= estimate(109 tax:0.1)
# 121   <= estimate(109 tax:0.1 service:10)
```

Type specialization
```
boolify a: a bool
boolify v
| string = v.size > 0
| array  = v.size > 0
| dict   = v.size > 0
| int    = v != 0
| float  = v != 0 && v.between(float.min float.max)
| _      = fail("compile error")
```

Struct
```
vector2:
  x int
  y int
  show v = `($v.x,$v.y)`

dict k v: kvs [k,v]
```

Enum
```
abc: | a | b | c
cupon:
| zero
| prize name string, price int
  show c =
  | zero = "zero"
  | prize = `$c.name $c.price`

either l r: | left l | right r
```

Type class
```
addable t::
  zero: t
  +: t t t

vector1:
  x int

vector1.addable:
  zero = vector1(0)
  + l r = vector1(l.x + r.x)
```





## 4. Namespace

Define name space
```
- math: _
pi = p

- private
p = 3.141592653589793
```

Use name space
```
- main: io math

main = io.print(math.pi)
```





## 5. Buildin

Reserved words
types
- bool, true, false
- int, float, string
- seq, array, set, dict, tuple, func
- opt, nil, do, try, error
- any, void
- i8, i16, i32, i64
- u8, u16, u32, u64
- f8, f16, f32, f64
methods
- trace
values
- this
- io, eff

Binary operators order
```
* // / %                # number (high)
+ -                     # number (low)
> >= < <=  == !=        # comparision (high)
|| && | &               # comparision (low)
:= += /= *= /= %= .= <- # effect
,                       # no effect, just for readability
```

### IO

Standard input, output and error
```
main =
  line <- io.readline
  line.empty
  | true = io.puts(line)
  | false = io.warn(line)
```

File system
```
main =
  files <- io.fs.ls("/tmp").filter(.is_file)
  contents <- files.map(file => file.read)
```

TCP
```
main = io.tcp.listen("127.0.0.1:8080" _handle):
_handle from =
  target <- from.readline
  to <- io.tcp.connect(target)
  from.bidirectional(to)
```

UDP
```
main = io.udp.bind("127.0.0.1:1234" _handle):
_handle packet =
  target <- packet.string.lines.first
  io.udp.sendto(target packet)
```

Async and Cancel
```
main =
  t <- io.async(io.sleep(10))
  io.sleep(1)
  t.done
  | _
  | t.cancel
  t.done.else(t.cancel)
```

Random
```
main =
  n <- io.random.int(1 3)
  io.exit(n)
```

Time
```
main =
  now <- io.now
  io.print(now)
```





## 6. Appendix

Design of Moa language
- List bounds check
- Linear logic resource management
- Threads and STM

Pending ideas
- Global variables
- Pointer
- Weak reference
- Using up many core
- Asynchronous IO
- Watch dog timer
- Logger, debugger, profiler and resources monitor
- Strong composability
- Preconditions and Postconditions

Rejected ideas
- Null
- Exception
- Shadowing
- Default mutable
- Allocate / free
- Macro
- Class
- Interface
- Out of range access
- Zero division
- Standard library
- GC
- Destructor
- Finalizer
- Globalization
- Type level programming

Syntax
```
root: def (br def)*
def:
| "- " ref : ref+ # define namespace
| "- " ref        # use namespace
| ref+ ":" type+  # signature
| ref+ ":" iattr+ # struct
| ref+ ":" itag+  # enum
| func
func: id arg* "=" body
attr: id type
tag : ref (attr ("," attr)*)?
ifunc: indent func
iattr: indent attr
itag: indent tag
body:
| stmt
| exp helper?
stmt: (indent exp)+
helper: : iattr* ifunc+
exp: formula branch?
branch: (indent "|" unit " -> " exp)* # pattern match
formula:
| unit op2 formula # op2    : v + 1
| ref op2u formula # update : n += 1
| unit
unit:
| value
| id ("." id | "(" formula* ")")*
| "(" exp ")"
| "(" id+ => body ")" # lambda : (x y => x + y)
| "(" unit+ ")"       # tuple  : (1 n)
| "(" kv+ ")"         # struct : (name="value" age=30+7)

value:
| int    # 1
| float  # 1.0
| string # "hi"
| bool   # true
kv  : id ":" formula
id  : [a-z0-9_]+
ref : id (. id)*
type: ref ("(" ref+ ")")?
op2 : [; + - * / % & | << >> + - > >= < <=  == != || &&]
op2u: [= := += /= *= /= %=]
indent: "\n  "
br: "\n  "
```

All single symbols

```
--- single ----------------------------
_    # ignore
+-/* # arithmetic
%    # mod
&    # intersection
|    # union
> >= # bool operator
< <= # bool operator
=    # function
"    # string
`    # string with variables evaluation
#    # comment
,    # reserved to separete inside () and {}
.    # glue two objects
\    # escape

--- argument --------------------------
:    # named
=1   # default
*    # variables
+    # more than 1 variables

--- group -----------------------------
()   # priority, tuple, struct
[]   # array
{}   # dictionary

--- not decided yet -------------------
;    # glue two statements
~    # -
$    # -
?    # -
!    # -
@    # -
&    # -
^    # -
'    # char?
```





## 7. TODOs
- [x] syntax for signature, struct, enum, visibility and namespace
    # signature
      pi: float
      id a: a a
      find k v: dict(k v) k v
    # struct
      vector2:
        x int
        y int
      array a:
        values [a]
      dict k v:
        values [k,v]
    # enum
      bool:
      | true
      | false
      maybe a:
      | just a
      | none
    # type class
      eq t:
        eq: t bool
      functor t:
        fmap u: (t u) u
      num t:
        +: t t
        -: t t
        *: t t
    # visibility
      type:
        field int
        - internal
        debug = `$field $magic`
        - private
        magic = 1
      - private
      const = 1
    # namespace
      - math: _
      pi = 1
      - m = math@v3.1
      - main: math
      puts(m.pi + math.pi)

- [x] dictionary syntax
    {k1 v1 k2 v2}
    {k1:v1 k2:v2}
- [x] struct syntax
    (a:1 b:2 c:(x => x))
- [x] extra arguments: variable, named, optional, default
    # variable
    sum a.num: a+ a
    sum ary = ary.reduce(0 (+))
    # named
    copy: from:string to:string io
    copy from to = io.fs.copy(from=from to=to)
    # optional
    pow: int int int=1 int
    pow a b m = (a ** b) % m
    # optional named
    mail: from:string to:string text:string subject:string="no title" html:string?=none
    mail from to text html subject = html
    | sendmail(from to subject text html=html)
    | sendmail(from to subject text)
- [] core signatures for v0
    # int
    # string
    - slice
    - replace
    - int
    # bool
    - if
    # array
    - size
    - has
    - slice
    - map
    - keep
    - sort
    # map
    # any
    - string
    # opt a
    - or a: opt(a) a a
    - then a b: opt(a) (a b) a
    # err a
    # io a
- [] core signatures for v1
    # int
    - hex
    - base int int
    # string
    - size
    - slice
    - replace
    - int
    # bool
    - if
    - int
    # bytes
    to a: opt(a)
    # array
    - size
    - has
    - slice
    - map
    - keep
    - sort
    # map
    - keys
    - values
    - size
    # any
    - string
    - bytes
    # opt a
    - or a: opt(a) a a
    # err a
    # io a
    # failable: opt, err, io
    - then a b: failable(a) (a b) failable(b)
- [] compile to JavaScript
- [] self booting by compiled JavaScript
- [] make memo app
- [] proposal: checked numbers
     - ci8, ci16, ci32, ci64
     - cu8, cf16, cu32, cu64
- [] proposal: 
     | "[" unit+ "]"       # array : [1 2]
     | "{" vv1* "}"        # dict2 : {x:1 y:2}
     | "{" vv2* "}"        # dict1 : {1 2 k v}
