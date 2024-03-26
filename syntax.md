# Syntax
```
top: def | exp+
def: id id+ exp
exp:
| id ("," id+ )* "=>" exp | block
| op1? atom (op2 exp)?
| ":" block
block: ("\n  " line)+ | line


func: id arg* ":" exp+ | block
declare: id+ "::" arg+ type
struct: id+ "::" ("\n  " id exp)+
enum: id+ ":|" ("\n  " id exp)+
block: ("\n  " line)+ | line
exp:
| id ("," id+ )* "=>" exp | block
| op1? atom (op2 exp)?
atom: bottom (prop | call | index | slice)*
prop: "." (id | [0-9]+)
call: "(" exp* ")"
index: "[" exp+ "]"
slice: "{" (id ("=" atom)?)* "}"
bottom:
| "(" exp ")"               # 1 * (2 + 3)
| "{" (id ("=" exp)?)* "}"  # {x y z=0}
| "[" exp* "]"              # [1 2 3]
| "-"? [0-9]+ ("." [0-9]+)? # -1.2
| '"' [^"]* '"'             # "string"
| id
op1: [!-] | ".."
op2: [+-*/%<>|&^~=!]+ | ","
id: [A-Za-z_][A-Za-z0-9_]*
arg:
| id
| id "?"
| id "=" exp
type:
| id ("[" type+ "]")?
| "(" type+ ")"
| "[" type+ "]"
```

Key word
```
void
bool
int
float
string
time
re
bytes

tuple
struct
list
dict
set

num
ref
io

interface
extern

throw
catch

true
false
std

reserved: use module decimal array duration i8 i16 i32 i64 u8 u16 u32 u64 f16 f32 f64
```

Symbols
```
_                part of id
.                field access
..               variadic function
"                string
#                comment
( )              priority
[ ]              list?
{ }              struct?
! -              singular operator
&& ||            boolean operator
+ - * / % **     number operator
| & ^ ~ << >>    bit operator
< <= > >= == !=  comparing operator
=                update a variable
=>               lambda
,                separation of arguments
:                block
;                break line
? undefined
\ undefined
' undefined
$ undefined
@ undefined
` undefined
```



# Idea

- Number
```
"-"? [0-9]+ "e" [0-9]+              # 1e3          -> 100
"-"? "0x" [0-9a-fA-F_]+             # 0xff         -> 255
"-"? "0o" [0-7_]+                   # 0o11         -> 9
"-"? "0b" [0-1_]+                   # 0b11         -> 3
"-"? [0-9][0-9_]+ ("." [0-9_]+)?    # 10_000.1_002 -> 10000.1002
"-"? ([0-9]+ ([zptgmk] "b" | "b"))+ # 2kb1b        -> 2049
```

- Duration
```
"-"? ([0-9]+ ("d" | "h" | "m" | "s" | "ms" | "us"))+ # 1h2m3s -> duration(hour=1 minute=30 second=3)
```

- Variadic function
```
def f a?: a            # f(1) is opt[int], f() is opt[void]
def f a=1: a           # f() or f(1)
def f ...a: a.max.1    # f(), f(1) or f(1 2)
def f ...a,: a.max.1   # f(), f(1 "a"), f(1 "a" 2 "b")
```

- Pass through
```
def show ...a: print ...a
```

- Named argument
```
def f {a}    : a  # f(a=1)
def f {a=0}  : a  # f() or f(a=1)
def f {a b}  : a  # f(a=1 b=2) or f(b=2 a=1)
def f {a b=0}: a  # f(a=1), f(a=1 b=2) or f(b=2 a=1)
```

- Implicit type converting for constant
```
1 + 2   # int
1 + 2.0 # float
```

- Pattern matching
```
case: "case" exp ":" ("\n  " const | pattern)
const: exp ("or" exp)* ":" block
pattern: matcher ("if" exp) "=>" block
matcher:
| '"' [^"]* '"'                 # string
| "-"? [0-9]+ ("." [0-9]+)?     # number
| "[" matcher* ("..." id?)? "]" # list
| "{" capture+ "}"              # struct
| capture
capture:
| id "." type "(" pattern* ")"  # type
| id "=" matcher
| id
```

```
enum tree t:
  leaf
  node t tree(t) tree(t)
def validate t:
  case t:
    leaf: true
    _.node(m _.leaf _.leaf) => true
    _.node(m l.node _.leaf) => l.0 <= m
    _.node(m _.leaf r.leaf) => m <= r.0
    _.node(m l.node r.node) => l.0 <= m <= r.0 && validate(l) && validate(r)

enum tree t:
  leaf
  node:
    value t
    left tree t
    right tree t
def validate t:
  case t:
    leaf: true
    {value left.leaf right.leaf} => true
    {value left.node right.leaf} => left.value <= value && validate(left)
    {value left.leaf right.node} => value <= right.value && validate(right)
    {value left.node right.node} => left.value <= value && value <= right.value && validate(left) && validate(right)
```


- typed argument
```
dec f int int
def f a: a      # int int

dec f float float float
def f a b: a / b  # float float float

dec f t.num: t t t
def f a b: a + b  # t.num => t t t

dec f t: list(t) t? t
def f lst alt?: if lst.size == 0 alt.or(t()) lst[0] 
```

- Core syntax
```
top: list | atom
list: "(" top* ")"
atom: id | num | string | op
id: [_A-Za-z][0-9_A-Za-z]*
num: "-"? [0-9] ("." [0-9])?
string: '"' [^"]* '"'
op: [-+*/%|&<>!=^~]+ | ":="
```

- Syntax sugar
```
a b        # 50%: (a b)
a.b        # 60%: (. a b)
a.b c d    # 55%: ((. a b) c d)
a.b(c d)   # 77%: ((. a b) c d)
a + b      # 60%: (+ a b)
a: b       # 50%: (a (b))
a b: c d   # 62%: (a b (c d))
a:         # 52%: (a (b (c d)))
  b
  c d
```
