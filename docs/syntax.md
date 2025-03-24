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
| "[" exp* "]"                            # [1 2 3] -> vec(0 3).push(1 2 3)
| "-"? [0-9]+ ("." [0-9]+)? ("e" [0-9]+)? # -1.2
| "-"? "0x" [0-9a-fA-F_]+                 # 0xff -> 255
| '"' [^"]* '"'                           # "string"
| '"""' [^"]* '"""'                       # """a="b"""" -> "a=\"b\""
| id "?"?
op1: [!-~] | "..."
op2: [+-*/%<>|&^=!]+
id: [A-Za-z_][A-Za-z0-9_]*
comment: "//" [^\n]*
```

Operator
```
! - ~                             # Unray
* ** / % + - << >> & ^ |          # number
== != < <= > >=                   # Compare
&& ||                             # Boolean
= += -= *= /= %= &= |= ^= <<= >>= # Override
```

Keyword
```
literal   : ... void true false some none
primitive : void bool int str fn i8 i16 i32 i64 u8 u16 u32 u64 f16 f32 f64
container : opt tuple struct set vec map
declare   : let var def class enum dec interface
branch    : iif if else guard match
flow      : return throw catch
loop      : for each while continue break
global    : log assert
std       : byte bytes regexp time duration stream decimal
reserved  : __.* duration num decimal import export
```

Symbols
```
( )  # priority
[ ]  # vec
.    # field access
_    # part of id
...  # variadic function or there are zero or more
"    # string
#    # comment
:    # block
=>   # lambda
,    # argument separator of lambda
::   # reserved for `where` of Haskell
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
map("a" 1 "b" 2)

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
```

# Ideas

idea: Optional argument
```
dec str.slice: str int int? int? str
def str.slice
| count           : ...
| start count     : ...
| start count step: ...
def f a?: a    # f() -> _, f(1) -> int, a is opt[T] in the function
def f a=1: a   # f() -> f(1)

(a,b=1 => a + b)(1) # 2
```

idea: Named argument
```
def f a{}    : a     # f(a=1 b=2) -> vec[tuple[str int]]
def f a{}    : g a   # f(a=1) -> g(a=1)
def f {a}    : a     # f(a=1) -> 1
def f {a?}   : a     # f() or f(a=1)
def f {a=0}  : a     # f() or f(a=1)
def f {a b}  : a     # f(a=1 b=2) or f(b=2 a=1)
def f {a b=0}: a     # f(a=1), f(a=1 b=2) or f(b=2 a=1)
def f {a}?   : a     # f() -> opt[_], f(a=1) -> 1
def f a {b}  : a + b # f(1 b=2) -> 3
```

idea: Loop
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

idea: Typed argument
```
dec flatten a                 : vec[array[a]] vec[a]
def flatten[t] a.vec[vec[t]]: a.map(x => x)
def flatten a: a.map(x => x)

dec sum t{+[t t t]}   : vec[t] t
def sum[t{+[t t t]}] a.vec[t]:
    let n t: each m a: n += m
def sum[t] a.vec[t]:
    let n t.zero: each m a: n += m
def sum a:
    let n: each m a: n += m

dec nth t: map[int t] int t!
def nth[t] d.map[int t] n.int:
    d[n]
def nth d n:
    d[n]
```

idea: Method
```
class vector2:
  x int
  y iny
  show1 v: "({}, {})".format v.x v.y

def vector2.show2 v:
  "({}, {})".format v.x v.y

def f:
  vector2(1 2).show1 # (1, 2)
  vector2(1 2).show2 # (1, 2)
```

idea: Interface
```
interface num t:
  (+ -) t t t

class vector1:
  x int

class vector1.num:
  + l r: vector1 (l.x + r.x)
  - l r: vector1 (l.x - r.x)
```

idea: Duration
```
"-"? ([0-9]+ ("w" | "d" | "h" | "m" | "s" | "ms" | "us" | "ns"))+ # 1h2m3s -> duration(hour=1 minute=30 second=3)
```

idea: Regexp
```
re"[0-9]+"
```

idea: Sugar
```
atom: bottom (... | copy | key)*
copy: "{" id* (id "=" atom)* "}"   # copy with new values
key: "." (id | [0-9]+) [!?] type?  # a.b! -> a.at("b"), a.b? -> a.get("b")
bottom: "{" id* (id "=" atom)* "}" # {x y=1}
```

idea: Where
```
def fib n:
  a.map(f):: # fib(5) -> [1 3 6 10 16]
    var sum 1
    let a [:n]
    def f x: sum += x
```

idea: Pattern matching
```
match: "match" exp ":" ("\n  " type? case ("if" exp) ":" block)+
case: pattern ("," pattern)*
pattern:
| '"' [^"]* '"'                    # string
| "-"? [0-9]+ ("." [0-9]+)?        # number
| "[" case* ("..." id?)? case* "]" # vec
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
    node{value=0}                   : false
    node{value} if value.isnan()    : false
    node{left=leaf right=leaf}      : true
    node{value left=node right=leaf}: left.value <= value && validate(left)
    node{value left=leaf right=node}: value <= right.value && validate(right)
    node{value left=node right=node}: left.value <= value <= right.value && validate(left) && validate(right)

match a:
  []: "empty"
  [n n _ _ _]: "pair"
  [n n m m m]: "fullhouse"
```

idea: Foreign function interface
```
def log n:
  __c.log(n) + __go.log(n) + __js.log(n)

def ps:
  __sh("ps").split("\n")

__moa
def log n: n ** n # fallback if the target language has no implementation

__c
#include <math.h>
double moa_log(double n) { return log(n); }

__go
import "math"
func Moa_log(n float64) float64 { return math.Log(n) }

__js
export const moa_log = n => Math.log(n)
```
