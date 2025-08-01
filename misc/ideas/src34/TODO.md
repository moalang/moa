# TODO
- [x] Implement minimal bootstrap
- [ ] Design minimal io{log}
- [-] Boot using JavaScript backend without any I/O
- [ ] Boot using C backend without any I/O
- [ ] Implement web application server mode
- [ ] Implement HTTP/1.1 server
- [ ] Implement transactional database

# Usage
```
Moa is an open-source programming language designed to improve the developer experience.

Usage: moa <command> [...arguments]

Commands:
  moa                     Start a shell
  moa build               Compile to an executable file
  moa run                 Run a file
  moa test                Run tests
  moa upgrade             Upgrade itself
  moa version             Display version
```

# Internal Syntax
```
atom:
| '"' ([^"\\]|\\.)* '"'
| [^ \n]+
| '(' atom* ')'
```

# Syntax sugar
```
a b       -> (a b)
a()       -> (a)
a(b)      -> (a b)
!a        -> (! a)
a + b     -> (+ a b)
a.b       -> (. a b)
a[b]      -> (__[ a b)
a[b] = c  -> (= (__[ a b) c)
[a b]     -> ([ a b)
{
  a
  b c
} -> (do a (b c))
```

Operator
```
! - ~                             # Unray
* ** / % + - << >> & ^ |          # number
== != < <= > >=                   # Compare
&& ||                             # Boolean
= += -= *= /= %= &= |= ^= <<= >>= # Override
===                               # Type check
```

Keyword
```
literal   : ... _ true false some none
primitive : void bool int str fn i8 i16 i32 i64 u8 u16 u32 u64 f16 f32 f64 byte bytes
container : opt tuple new set vec map
declare   : let var def class enum dec interface
branch    : iif if else match when
flow      : return throw catch
loop      : for each while continue break
test      : assert
std       : regexp time duration stream decimal
reserved  : __.* num import export
```

Symbols
```
( )  # priority
[ ]  # vec
{ }  # block
:    # block
.    # field access
_    # part of id
...  # variadic function or there are zero or more
"    # string
#    # comment
=>   # lambda
,    # argument separator of lambda
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
vec(1 2)
set(1 2)
map("a" 1 "b" 2)

a => a
[1 2]

let a 1: a += 1
var a b c 0
dec f a: a a bool
def f x y: x == y
dec g a.num: ...a a
def g ...xs: xs.fold(+)
dec h: ... void
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
catch throw(3) fn(e: log e.message e.stack)
catch throw(b(1)) e => match e.data:
  a: "a"
  b v: "b {}".format(v)
  c v: "c {}".format(v)

if n < 0: return 0

for 3: continue
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
for 3: ...                         # 3 times
for i 3: ...                       # 0 1 2
for i 1 < 3: ...                   # 1 2
for i 2 >= 0: ...                  # 2 1 0
for i 1 <= 5 +=2: ...              # 1 3 5
for i 1,3 <= 5: ...                # 1 3 5
each x xs: ...                     # each item
each i x xs: ...                   # each item with index
while l < r: ...                   # while
for i n: for j m: break.i          # nested break from for
each xs x: each ys y: break.x      # nested break form each
while.z l < r: while m: continue.z # nested continue with label
```

idea: Typed argument
```
dec flatten a            : vec[vec[a]] vec[a]
def flatten a.vec[vec[_]]: a.fmap(x => x)
def flatten a: a

dec sum t{+[t t t]}: vec[t] t
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
  tuple: tuple x y # property
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
r/[0-9]+/
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

idea: Return for
```
def odds n:
  return for i n:
    if i % 2 == 0:
      continue
    i
```

idea: Pattern matching
```
match: "match" exp ("," exp)* ":" case
case:
| "\n  "matcher ("if" exp)? ":" block
| "\n  "matcher "if:" ("\n    " exp ":" block)
| "\n  "matcher "match" match
matcher: pattern ("," pattern)*
pattern:
| "(" top ")"                          # group
| "[" bottom* ("..." id?)? bottom* "]" # vec
| "{" (id ("=" bottom)?)+ "}"          # class
| type ("(" id? pattern? ")")?         # type
| '"' [^"]* '"'                        # string
| "-"? [0-9]+ ("." [0-9]+)?            # number
type: id ("." id)* ("[" type+ "]")?

enum tree t:
  leaf
  node:
    value t
    left tree t
    right tree t

def validate t:
  return match t:
    leaf: true
    node n match n.left, n.right:
      leaf, leaf: n >= 0
      node, node: n.left.value <= n.value <= n.right.valuer && validate(n.left) && validate(n.right)
      node, leaf: n.left.value <= n.value                   && validate(n.left)
      leaf, node:                 n.value <= n.right.value  && validate(n.right)

def validate t:
  return match t:
    leaf: true
    node n if:
      n.left === leaf && n.right === leaf: n >= 0
      n.left === node && n.right === node: n.left.value <= n.value <= n.right.valuer && validate(n.left) && validate(n.right)
      n.left === node && n.right === leaf: n.left.value <= n.value                   && validate(n.left)
      n.left === leaf && n.right === node:                 n.value <= n.right.value  && validate(n.right)
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
