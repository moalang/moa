# Core Syntax
```
top: line+
line: exp+ (":" block)? comment? "\n"
block: ("\n  " line)+ | line
exp: op1? atom (op2 exp)?
atom:
| "(" exp ")"
| bottom (prop | call | copy)*
prop: "." (id | [0-9]+)       # property access
call: "(" exp* ")"            # call function
index: "[" exp+ "]"           # index access or generic
bottom:
| "(" exp ")"                 # 1 * (2 + 3)
| "[" exp* "]"                # [1 2 3] -> list(1 2 3)
| "-"? [0-9]+ ("." [0-9]+)?   # -1.2
| "-"? "0x" [0-9a-fA-F_]+     # 0xff    -> 255
| '"' [^"]* '"'               # "string"
| '"""' [^"]* '"""'           # """a="b""""     -> "a=\"b\""
| id [?]?
op1: [!-~] | "..."
op2: [+-*/%<>|&^=!]+
id: [A-Za-z_][A-Za-z0-9_]*
comment: "//" [^\n]*
```

Keywords
```
literal   : any true false some none
primitive : bool int float string fn error i8 i16 i32 i64 u8 u16 u32 u64 f16 f32 f64
container : option tuple list set dict
declare   : let var def class enum dec interface extern
branch    : iif if else switch
flow      : return throw catch for each while continue break
global    : log assert
```

Reserved
```
bytes regexp time duration stream num decimal array
use module
_ __[.*]
```

Operators
```
! - ~           # Unray
|| &&           # Boolean
+ - * ** / %    # Arithmetic
& | ^ << >>     # Bit
== != < <= > >= # Compare
=               # Update
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
{ }  # reserved for class
=>   # reserved for lambda
,    # reserved for argument separator of lambda
;    # undefined
?    # undefined
\    # undefined
'    # undefined
$    # undefined
@    # undefined
`    # undefined
```

Loop
```
for 2: log 1              # 1 1
for 2 x => log x          # 0 1
for 1 3 x => log x        # 1 2
for 1 4 2 x => log x      # 1 3
for i 2 0 (-1) x => log x # 2 1
each [1 2] x => log x     # 1 2
while a < b: c
```

Enum switching
```
switch: "switch" exp ":" ("\n  " type id? ":" block)+
type: id ("." id)* ("[" type+ "]")? ("(" case ")")?

enum ab t:
  a
  b int

def show t:
  switch t:
    a: "a"
    b n: b.string
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
def f ?a: a         # f() returns option[_], f(1) returns option[int]
def f a=1: a        # f() is f(1)
def f ...a: a       # f(), f(1) or f(1 2)
def f ...a,: a      # f(), f(1 "a"), f(1 "a" 2 "b")
def f ...a,,: a     # f(), f(1 "a" true), f(1 "a" true 2 "b" false)
def f ...a: g(...a) # f(1 2) will call g(1 2)
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

dec sum t{.zero t; +: t t t}   : list[t] t
def sum[t{.zero t; +: t t t}] a.list[t]:
    let n t.zero: each m a: n += m
def sum[t] a[list[t]]:
    let n t.zero: each m a: n += m
def sum a:
    let n typeof(a).0.zero: each m a: += m

dec product t{.one t; *: t t t}: list[t] t
def product[t{.one t; *: t t t}] a[list[t]]:
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
copy: "{" id* (id "=" atom)* "}"   # copy with updates
key: "." (id | [0-9]+) [!?] type?  # a.b!            -> a.at("b"), a.b? -> a.get("b")
bottom:
| "[" ":" | (atom ":" atom)+ "]"   # ["x":1 ("y"):2] -> dict("x" 1 "y" 2)
| "{" id* (id "=" atom)* "}"  # {x y=1}
| "-"? [0-9]+ "e" [0-9]+           # 1e3             -> 100
| "-"? "0o" [0-7_]+                # 0o11            -> 9
| "-"? "0b" [0-1_]+                # 0b11            -> 3
| "-"? [0-9][0-9_]+ ("." [0-9_]+)? # 10_000.1_002    -> 10000.1002
```

idea: Pattern matching
```
switch: "switch" exp ":" ("\n  " type? case ("if" exp) ":" block)+
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
  switch t:
    leaf: true
    node {value left.node right.leaf}: left.value <= value && validate(left)
    node {value left.leaf right.node}: value <= right.value && validate(right)
    node {value left.node right.node}: left.value <= value <= right.value && validate(left) && validate(right)
```

idea: Error handling
```
catch: "catch" exp id? ":" ("\n  " type id? ("if" exp) ":" block)+
type: id ("." id)* ("[" type+ "]")? ("(" case ")")?

def calc f:
  catch f():
    int n if n == 0: "zero"
    int n: n.string()
    _: ""
```
