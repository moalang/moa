# Syntax
```
top: line+
line: exp+ (":" block)?
block: (("\n  " line)+ | exp+)
exp: op1? unit (op2 exp)?
unit: bottom (prop | call)*
prop: "." [0-9A-Za-z_]+
call: "(" exp* ")"
bottom:
| "(" exp ")"               # 1 * (2 + 3)
| "[" exp* "]"              # [1 2 3]
| "{" (id ("=" unit)?)* "}" # {x y z=0}
| [0-9]+ ("." [0-9]+)?      # 1.2
| '"' [^"]* '"'             # "hi"
| id ("," id)* "=>" block   # a,b => a + b
| id
op1: [!-]
op2:
| [+-*/%<>|&^~=,]
| [+-*/%<>|&^~=!] "="
| ("**" | "++" | "&&" | "||") "="?
| ">>" | "<<"
id: [A-Za-z_][A-Za-z0-9_]*
reserved1: let var def class if match for while yield return throw catch test use module
reserved2: bool int float string tuple struct list set dict option
reserved3: true false some none
reserved4: time log http cache orm sql
reserved5: i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 num bytes union interface implement === !==
```



# Symbols
```
_                part of id
.                field access
...              variadic function
"                string
#                comment
( )              priority
[ ]              list
{ }              struct
! -              singular operator
&& ||            boolean operator
+ - * / % **     number operator
| & ^ ~ << >>    bit operator
< <= > >= == !=  comparing operator
=                update a variable
,                tuple
:                block
?                optional type or argument
\                escape a character
' undefined
$ undefined
@ undefined
; undefined
` undefined
```



# Idea
- Numbers
  "0x" [0-9a-fA-F_]+          # 0xff           -> 255
  "0o" [0-7_]+                # 0o11           -> 9
  "0b" [0-1_]+                # 0b11           -> 3
  [0-9][0-9_]+ ("." [0-9_]+)? # 10_000.1_002   -> 10000.1002

- Variadic function
  def f a=1: a         # default value
  def f a? : a.or(1)   # zero or one
  def f a* : a.max     # zero or more
  def f a+ : a.max     # one or more
  def f a,+: a.max.1   # two, four or more : a is list[tuple[t u]]

- Named argument
  def f {a}    : a     # f(a=1)
  def f {a=0}  : a     # f() or f(a=1)
  def f {a b}  : a     # f(a=1 b=2) or f(b=2 a=1)
  def f {a b=0}: a     # f(a=1), f(a=1 b=2) or f(b=2 a=1)

- Implicit type converting for constant
  1 + 2   # int
  1 + 2.0 # float

- Pass through
  def show ...a: print ...a
  def show ...: print ...

- Pattern match
  "match " exp ":" ("\n  " pattern (if exp)? ":" exp)+
  pattern: matcher ("|" pattern)*         # a | b   : or
  matcher:
  | '"' [^"]* '"'         # "s"                          : string
  | [0-9]+ ("." [0-9]+)?  # 1.2                          : number
  | "true" | "false"      # true                         : bool
  | "[" pattern* "]"      # [a b]                        : list
  | "{" (capture "}"      # {value left.leaf right.leaf} : struct
  | capture               # x.leaf                       : union
  capture: id ("." id)*

- typed argument
  def f a.int .int      : a     # int int
  def f a b.float       : a / b # float float float
  def f t.num => a.t b.t: a + b # t.num => t t t
