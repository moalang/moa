# BNF like definition of syntax
top: line*
line: keyword? exp+ block?
block: ":" (("\n  " line)+ | exp)
exp: op1? unit (op2 exp)?
unit: bottom (prop | call | list | dict | struct)*
prop: "." id
call: "(" exp* ")"                           # f(a b=c)
list: "[" exp* "]"                           # a[1]
dict: "[" ":" | (unit ":" unit)+ "]"         # a["a":b c:1]
struct: "{" (id ("." unit)? ("=" exp)?)* "}" # a{a b=1}
bottom:
| "(" exp ")"                     # 1 * (2 + 3)  : priority
| " ." id                         # .int         : type
| [0-9]+ ("." [0-9]+)?            # 1.2          : number
| '"' [^"]* '"'                   # "s"          : string
| (id ("," id)*)? "=>" exp        # a,b => a + b : lambda
| list | dict | struct
| id
op1: [!-]
op2:
| [+-*/%<>|&^=,]                  # (1, "hi")   -> tuple 1 "hi"
| [+-*/%<>|&^=] "="
| "**" | "&&" | "||" | ">>" | "<<"
| "===" | "**=" "<=>"
id: [a-za-z_][a-za-z0-9_]*
keyword: define | branch
define: def var let struct union test
branch: iif match
statement: if else for each while test return yield continue break throw catch
primitive: bool true false int float nan inf num string list set dict tuple option some none time
reservation: ref many deft use module interface implement bytes iter lazy array assert i8..i64 u8..u64 f32 f64 decimal



# Syntax sugger for value
| '"""' [^"]* '"""'           # """a"b"""      -> "a\"b"
| $'"' [^"]* '"'              # "s {a} {b 04}" -> "s {} {04}".format(a b)
| id ("," id)+ "=" exp        # a, b = c       -> a = c.0; b = c.1; c
| bottom "[" exp+ "]" "=" exp # a[1] = 2       -> a.get(1).set(2)
| "0x" [0-9a-fA-F_]+          # 0xff           -> 255
| "0o" [0-7_]+                # 0o11           -> 9
| "0b" [0-1_]+                # 0b11           -> 3
| [0-9][0-9_]+ ("." [0-9_]+)? # 10_000.1_002   -> 10000.1002
| [0-9]+ or(sec second seconds min minute minutes hour hours day days month months year years) # into int



# Semantics
- Pattern match
  "match(" exp "):" ("\n  " pattern (if exp)? ":" exp)+
  pattern: matcher ("," pattern)?              # a, b    : tuple
  type: ("." id prop*)+
  capture: id type?
  matcher:
  | '"' [^"]* '"'                              # "s"           : string
  | [0-9]+ ("." [0-9]+)?                       # 1.2           : number
  | "true" | "false"                           # true          : bool
  | "[" pattern* "]"                           # [0 x]         : list
  | "[:]" | "[" (pattern ":" pattern)+ "]"     # ["0":a b:"1"] : dict
  | "(" pattern ")"                            # (a, b)        : priority
  | type term?                                 # .type         : type match
  | capture term?                              # id.type       : capture and type match
  term:
  | "{" (capture ("=" exp | term)?)* "}"       # {a b.int c=1 d[0] } : match structure
  | "[" (type | capture)+ "]"                  # [.int t]            : match type parameters

- Implicit type converting
  1 + u8(2)               # u8
  a -> lazy[a] <-> def[a] # ft f a: lazy[a] a; def f f: print(f() f.string); def g: f(1 + 2)
  many[a] -> list[a]      # ft f a: many[a] a; def f a*: a.sum; def g: f 1 2

- inline if
  iif a b c
  iif:
    a: 1
    b: 2
    3

- typed argument
  def f a.int .int: a         # int int
  def f a b.float: a / b      # float float float
  def f t.num => a.t b.t: a + b # t.num => t t t

- Variable length argument
  def f a=1: a        # zero or one with default
  def f a*: a.sum     # zero or more
  def f a+: a.sum     # one or more
  def f a*: dict a... # pass throw
  def f               # zero, one or three
  | 0
  | a: a
  | a b: a + b

- Named argument
  def f {a}: a        # f(a=1)
  def f {a=0}: a      # f() or f(a=1)
  def f {a b}: a      # f(a=1 b=2) or f(b=2 a=1)
  def f {a b=0}: a    # f(a=1) or f(a=1 b=2) or f(b=2 a=1)

- Macro
  def until f.lazy g.lazy: while f(): g() # f and g are not evaluated at caller

# Symbols
_                  part of id
.                  access an element of object
"                  string
`                  string with variables
#                  comment
! -                singular operator
&& ||              binary operator for boolean
>> <<              bit shift for integer
+ - * / % **       binary operator for number
| & ^              binary operator for int(or, and, xor)
( )                priority
[ ]                list or dict
{ }                object
< <= > >= == ===   comparing
=                  update existing a variable
,                  delimiter to tuple and arguments in anonymous function
:                  delimiter to start indented block
? undefined
\ undefined
@ undefined
~ undefined
' undefined
$ undefined
; undefined



# Basic usage
- Primitive
  - bool           # true, false
  - int            # 1, -1
  - float          # 1.0, -1.0
  - string         # "hi"
  - lambda         # a,b => a + b
  - bytes          # bytes(1024)
- Container
  - list           # [1 2]               list[int](1 2)
  - set            # set(1 2)            set[int](1 2)
  - dict           # ["a":1 b:1+2]       dict[string int]("a" 1 b 1+2)
  - tuple          # 1,"hi"              tuple[int string](1 "hi")
  - struct         # {a:a f:x,y=>g(x y)} __new(a f x,y => g(x y))
- Definition
  - variable       # var a 1
  - constant       # let a 1
  - function       # def f a: a
  - struct         # struct a: b c
  - union          # union a: b; c d; e: f g
- Statement
  - if / else      # if a: b; else if c: d; else: e
  - return         # return 1
  - match          # match a:; b: c; _: d
  - throw / catch  # def f: throw "..."; catch f; a.t: a; b.u: b; c: c
  - for            # for i 9: n += i
  - each           # each x xs: n += x
  - while          # while a: b
  - continue       # continue
  - break          # break
  - yield          # def f: yield 1; yield 2 # iter[int]
- Misc
  - iif            # iif a b c d e   ->   a ? b : c ? d : e
  - module         # module a: inc int int; use a inc
  - comment        # # comment
  - test           # test t: t.eq "hi" greet



# Advanced usage
union ast:
  real float
  op2:
    op string
    lhs ast
    rhs ast
def eval a:
  match a:
    x.real: x
    x.op2{op="+"}: eval(x.lhs) + eval(x.rhs)
    x.op2{op="-"}: eval(x.lhs) - eval(x.rhs)
    x: throw $"unknown {x}"

union tree a:
  leaf
  node:
    value a
    left tree[a]
    right tree[a]
def validate t:
  match t:
    .leaf                       : true
    n.node if n.value == nan    : false
    n.node{left.node right.node}: left.value <= n.value <= right.value && validate(left) && validate(right)
    n.node{left.node}           : left.value <= n.value && validate(left)
    n.node{right.node}          : n.value <= right.value && validate(right)
    # else is not needed because the above covers all
