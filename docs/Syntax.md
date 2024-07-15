# Core Syntax
```
top: line+
line: exp+ (":" block)? comment? "\n"
block: ("\n  " line)+ | line
exp: op1? atom (op2 exp)?
ato:
| "(" exp ")"
| bottom (prop | call)*
prop: "." (id | [0-9]+)     # property access
call: "(" exp* ")"          # call function
bottom:
| "(" exp ")"               # 1 * (2 + 3)
| "-"? [0-9]+ ("." [0-9]+)? # -1.2
| '"' [^"]* '"'             # "string"
| id
op1: [!-] | "..."
op2: [+-*/%<>|&^~=!]+
id: [A-Za-z_][A-Za-z0-9_]*
comment: "//" [^\n]*
```

# Syntax sugar
```
exp:
| id ("," id+ )* "=>" exp | block  # a,b => c        -> fn(a b c)
| op1? atom (op2 exp)?
atom:
| "(" exp ")"
| bottom (prop | call | index | slice | key)*
key: "." (id | [0-9]+) [!?] type?  # a.b!            -> a.at("b"), a.b? -> a.get("b")
index: "[" ":"? exp+ "]"           # a[1]            -> a.at(1)
slice: "{" id* (id "=" atom)* "}"  # a{b c=1}        -> struct.copy(a b c=1)
bottom:
| "{" id* (id "=" atom)* "}"       # {x y=1}         -> struct(x y=1)
| "[" exp* "]"                     # [1 2 3]         -> list(1 2 3)
| "[" ":" | (atom ":" atom)+ "]"   # ["x":1 ("y"):2] -> dict("x" 1 "y" 2)
| '"""' [^"]* '"""'                # """a="b""""     -> "a=\"b\""
| "-"? [0-9]+ "e" [0-9]+           # 1e3             -> 100
| "-"? "0x" [0-9a-fA-F_]+          # 0xff            -> 255
| "-"? "0o" [0-7_]+                # 0o11            -> 9
| "-"? "0b" [0-1_]+                # 0b11            -> 3
| "-"? [0-9][0-9_]+ ("." [0-9_]+)? # 10_000.1_002    -> 10000.1002
```

Keywords
```
global    : true false some none time duration regexp log assert
primitive : bool int float string bytes stream fn
container : tuple struct option set list dict
declare   : let var def dec class enum
branch    : iif if else switch
flow      : return for while continue break try throw catch
reserved  : num decimal array use module i8 i16 i32 i64 u8 u16 u32 u64 f16 f32 f64
```

Operators
```
! -             # Unray
|| &&           # Boolean
+ - * ** / %    # Arithmetic
& | ^ ~ << >>   # Bit
!= == < <= >= > # Compare
=               # Update
```

Symbols
```
( )  # priority
[ ]  # list
{ }  # class
=>   # Lambda
.    # field access
_    # part of id
...  # variadic function or there are zero or more
"    # string
#    # comment
,    # seperator of arguments for lambda expression
:    # block
;    # break line
?    # undefined
\    # undefined
'    # undefined
$    # undefined
@    # undefined
`    # undefined
```



# Idea
Variadic function
```
def f a?: a           # f(1) is opt[int], f() is opt[void]
def f a=1: a          # f() is f(1)
def f ...a: a.max     # f(), f(1) or f(1 2)
def f ...a,: a.max.1  # f(), f(1 "a"), f(1 "a" 2 "b")
def f ...a,,: a.max.1 # f(), f(1 "a" true), f(1 "a" true 2 "b" false)
def f ...a: g(...a)   # f(1 2) will call g(1 2)
```

Named argument
```
def f {a}    : a  # f(a=1)
def f {a?}   : a  # f() or f(a=1)
def f {a=0}  : a  # f() or f(a=1)
def f {a b}  : a  # f(a=1 b=2) or f(b=2 a=1)
def f {a b=0}: a  # f(a=1), f(a=1 b=2) or f(b=2 a=1)
def f {a b}? : a  # f() or f(a=1 b=2)
```

Typed argument
```
dec f: int int
def f a: a

dec f: float float float
def f a b: a / b

dec f t.num: t t t
def f a b: a + b

dec f t: list(t) t? t
def f lst alt?: if lst.size == 0 alt lst[0]

# variadic argument with type declaration
dec f a: a? a
def f a: a.value

dec f: a?int a
def f a=1: a

dec f: ...int int
def f ...a: a.sum 

dec f a: ...int,a int
def f ...a: a.max.1

dec f a b: ...int,a,b int
def f ...a: a.max.1

dec f ...a: ...a a
def f ...a: g(...a)
```

Pattern matching
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

Method
```
struct vector2:
  x int
  y iny

def vector2.show v:
  "({}, {})".format v.x v.y

def f:
  vector2(1 2).show # (1, 2)
```

Type alias
```
struct seconds int

def seconds.milliseconds n:
  n * 1000

def f:
  seconds(3).milliseconds # 3000
```

Duration
```
"-"? ([0-9]+ ("d" | "h" | "m" | "s" | "ms" | "us" | "ns"))+ # 1h2m3s -> duration(hour=1 minute=30 second=3)
```

Regexp
```
re"[0-9]+"
```
