# Core Syntax
```
top: line+
line: exp+ (":" block)? comment? "\n"
block: ("\n  " line)+ | line
exp:
| op1? atom (op2 exp)?
| id ("," id+ )* "=>" block               # a,b => c        -> fn(a b c)
atom:
| "(" exp ")"
| bottom (prop | call)*
prop: "." (id | [0-9]+)                   # property access
call: "(" exp* ")"                        # call function
index: "[" exp+ "]"                       # index access or generic
bottom:
| "(" exp ")"                             # 1 * (2 + 3)
| "[" exp? ":" exp? "]"                   # [:3] -> [0 1 2]
| "[" exp* "]"                            # [1 2 3] -> list(1 2 3)
| "-"? [0-9]+ ("." [0-9]+)? ("e" [0-9]+)? # -1.2
| "-"? "0x" [0-9a-fA-F_]+                 # 0xff -> 255
| '"' [^"]* '"'                           # "string"
| '"""' [^"]* '"""'                       # """a="b"""" -> "a=\"b\""
| id [?]?
op1: [!-~] | "..."
op2: [+-*/%<>|&^=!]+
id: [A-Za-z_][A-Za-z0-9_]*
comment: "//" [^\n]*
```

Operator
```
! - ~           # Unray
|| &&           # Boolean
+ - * ** / %    # Arithmetic
& | ^ << >>     # Bit
== != < <= > >= # Compare
=               # Override
```

Keyword
```
literal   : _ true false some none
primitive : _ bool int float string fn error i8 i16 i32 i64 u8 u16 u32 u64 f16 f32 f64
container : option tuple list set dict
declare   : let var def class enum dec interface extern
branch    : iif if else guard match
flow      : return throw catch
loop      : for each while continue break
global    : log assert
reserved  : _ bytes regexp time duration stream num decimal array use module
```

Symbols
```
( )  # priority
[ ]  # list
.    # field access
_    # part of id
...  # variadic function or there are zero or more
"    # string
#    # comment
:    # block
=>   # lambda
,    # argument separator of lambda
{ }  # reserved for class
;    # undefined
?    # undefined
\    # undefined
'    # undefined
$    # undefined
@    # undefined
`    # undefined
```

Example
```
true
1
1.1
"a"
"""a"""
fn(1)()
fn(a a)(1)
fn(a b: a + b)(1 2)
tuple(1 "a").0
[1 2]
set(1 2)
dict("a" 1 "b" 2)

var a 1: a += 1
let a 1: a += 1
dec f a: a a bool
def f x y: x == y
dec g a.num: ...a int
def g ...ns: n.fold(+)
dec h: ... _
def h ...o: log ...o
class v2:
  x int
  y int
enum abc:
  a
  b int
  c: d bool

iif false 1 true 2 3
iif:
  a > b: c
  d    : e
  : f
if cond1: log 1
else if cond2: log 2
else: log 3
if:
  cond1: log 1
  cond2: log 2
  _    : log 3
catch throw(3) e => log e.message e.stack
catch throw(b(1)) e => match e.data:
  a: "a"
  b v: "b {}".format(v)
  c v: "c {}".format(v)

guard n < 0     # return returned_type()
guard n == 1: 1 # return 1
if n < 0: return 0

for i 3: continue
while true: break
each i x [5::-2]: log i x # 0 4 1 2 2 0 # TBD
```

IO [TBD]
```
sh('ls' '-alF') # string or exception
```

Loop [TBD]
```
for i 3: ...                       # 0 1 2
for i = 1 < 3: ...                 # 1 2
for i = 2 >= 0: ...                # 2 1 0
for i = 1 <= 5 +=2: ...            # 1 3 5
each x xs: ...                     # each item
each i x xs: ...                   # each item with index
while l < r: ...                   # while
for i n: for j m: break.i          # nested break with index
while.z l < r: while m: continue.z # nested continue with label # TBD
```



# Ideas

idea: Variadic argument
```
dec string.slice: string int int?     int?     string
dec string.slice: string int int=none int=none string
def string.slice
| count.int                   : ...
| start.int count.int         : ...
| start.int count.int step.int: ...
def f a?: a    # f() returns option[_], f(1) returns option[int]
def f a=1: a   # f() is f(1)

a,b=1 => a + b
```

idea: Named argument
```
def f a{}    : a  # f(a=1 b=2) returns list[tuple[string int]]
def f {a}    : a  # f(a=1)
def f {a?}   : a  # f() or f(a=1)
def f {a=0}  : a  # f() or f(a=1)
def f {a b}  : a  # f(a=1 b=2) or f(b=2 a=1)
def f {a b=0}: a  # f(a=1), f(a=1 b=2) or f(b=2 a=1)
def f {a b}? : a  # f() or f(a=1 b=2)
```

idea: Typed argument
```
dec flatten a                 : list[list[a]] list[a]
def flatten[t] a.list[list[t]]: a.map(x => x)
def flatten a: a.map(x => x)

dec sum t{.zero[t] +[t t t]}   : list[t] t
def sum[t{.zero[t] +[t t t]}] a.list[t]:
    let n t.zero: each m a: n += m
def sum[t] a[list[t]]:
    let n t.zero: each m a: n += m
def sum a:
    let n typeof(a).0.zero: each m a: += m

dec product t{.one[t] *[t t t]}: list[t] t
def product[t{.one[t] *[t t t]}] a[list[t]]:
    let n t.one: each m a: n *= m
def product[t] a[list[t]]:
    let n t.one: each m a: n *= m
def product a:
    let n typeof(a).0.one: each m a: n *= m

dec nth t: dict[int t] int t!
def nth[t] d[dict[int t]] n[int]:
    d[n]
def nth d n:
    d[n]
```

idea: Method
```
class vector2:
  x int
  y iny

def vector2.show v:
  "({}, {})".format v.x v.y

def f:
  vector2(1 2).show # (1, 2)
```

idea: Type alias
```
class seconds = int

def seconds.milliseconds n:
  n * 1000

def f:
  seconds(3).milliseconds # 3000
```

idea: Interface
```
interface num t:
  (+ - * / % **) t t t

class vector2:
  x int
  y int

class vector2.num:
   + l r: vector2 (l.x  + r.x) (l.y  + r.y)
   - l r: vector2 (l.x  - r.x) (l.y  - r.y)
   * l r: vector2 (l.x  * r.x) (l.y  * r.y)
   / l r: vector2 (l.x  / r.x) (l.y  % r.y)
   % l r: vector2 (l.x  / r.x) (l.y  % r.y)
  ** l r: vector2 (l.x ** r.x) (l.y ** r.y)
```

idea: Duration
```
"-"? ([0-9]+ ("d" | "h" | "m" | "s" | "ms" | "us" | "ns"))+ # 1h2m3s -> duration(hour=1 minute=30 second=3)
```

idea: Regexp
```
re"[0-9]+"
```

idea: Sugar
```
exp:
| id ("," id+ )* "=>" exp | block  # a,b => c        -> fn(a b c)
| op1? atom (op2 exp)?
atom:
| "(" exp ")"
| bottom (prop | call | index | copy | key)*
copy: "{" id* (id "=" atom)* "}"   # copy with new values
key: "." (id | [0-9]+) [!?] type?  # a.b!            -> a.at("b"), a.b? -> a.get("b")
bottom:
| "[" ":" | (atom ":" atom)+ "]"   # ["x":1 ("y"):2] -> dict("x" 1 "y" 2)
| "{" id* (id "=" atom)* "}"  # {x y=1}
```

idea: Pattern matching
```
match: "match" exp ":" ("\n  " type? case ("if" exp) ":" block)+
case: pattern ("," pattern)*
pattern:
| '"' [^"]* '"'                    # string
| "-"? [0-9]+ ("." [0-9]+)?        # number
| "[" case* ("..." id?)? case* "]" # list
| "{" ((id "=" exp) | type)+ "}"   # class
| type
type: id ("." id)* ("[" type+ "]")? ("(" case ")")?

enum tree t:
  leaf
  node:
    value t
    left tree t
    right tree t

def validate t:
  match t:
    leaf: true
    node {value left.node right.leaf}: left.value <= value && validate(left)
    node {value left.leaf right.node}: value <= right.value && validate(right)
    node {value left.node right.node}: left.value <= value <= right.value && validate(left) && validate(right)
```
