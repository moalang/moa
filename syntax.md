# BNF like definition of syntax
top: line*
line: keyword? exp+ block?
block: ":" (("\n  " line)+ | exp)
exp: op1? unit (op2 exp)?
unit: bottom (prop | call | list | dict | struct)*
prop: "." type
call: "(" exp* ")"                   # f(a b=c)
list: "[" exp* "]"                   # a[1]
dict: "[" ":" | (unit ":" unit)+ "]" # a["a":b c:1]
struct: "{" (id (":" unit)?)* "}"    # a{b:1}
bottom:
| "(" exp ")"                     # 1 * (2 + 3)  : priority
| " ." type                       # .t.fields    : type matcher
| [0-9]+ ("." [0-9]+)?            # 1.2          : number
| '"' [^"]* '"'                   # "s"          : string
| list | dict | struct
| (id ("," id)*)? "=>" exp        # a,b => a + b : lambda
| id
type: id ("[" id+ "]")? ("{" id ":" unit "}")?
op1: [!-]
op2:
| [+-*/%<>|&^=,]                        # (1, "hi")   -> tuple 1 "hi"
| [+-*/%<>|&^=] "="
| "**" | "&&" | "||" | ">>" | "<<"
| "===" | "**=" "<=>"
id: [a-za-z_][a-za-z0-9_]*
keyword: define | branch | statement
define: ft fn var let struct union test
branch: iif if else match throw catch
statement: for each while test return yield continue break
primitive: bool int float num string list set dict tuple true false nan inf ref many option
reservation: use module interface implement bytes iter lazy array assert i8..i64 u8..u64 f32 f64 decimal



# Syntax sugger for value
| '"""' [^"]* '"""'           # """a"b"""      -> "a\"b"
| $'"' [^"]* '"'              # "s {a} {b 04}" -> "s {} {04}".format(a b)
| id ("," id)+ "=" exp        # a, b = c       -> a = c.0; b = c.1; c
| bottom "[" exp+ "]" "=" exp # a[1] = 2       -> a.get(1).set(2)
| "0x" [0-9a-fA-F_]+          # 0xff           -> 255
| "0o" [0-7_]+                # 0o11           -> 9
| "0b" [0-1_]+                # 0b11           -> 3
| [0-9][0-9_]+ ("." [0-9_]+)? # 10_000.1_002   -> 10000.1002



# Semantics
- Pattern match
  "fn" id pattern*: ...
  "match" exp ":" ("\n  " pattern (if exp+)? ":" exp+)+
  pattern: matcher ("," pattern)?           # a, b    : tuple
  matcher:
  | "(" pattern ")"                         # (x)     : priority
  | '"' [^"]* '"'                           # "s"     : string
  | [0-9]+ ("." [0-9]+)?                    # 1.2     : number
  | id term?                                # x       : capture
  term:
  | ("." type)+                             # .t      : type
  | "{" id ("." type)? ("=" pattern)? "}"   # {a b.t} : struct
  | "[" pattern* "]"                        # [0 x]   : list
  | "[:]" | "[" (pattern ":" pattern)+ "]"  # ["s":x] : dict

- Implicit type converting
  1 + u8(2)              # u8
  a -> lazy[a] <-> fn[a] # ft f a: lazy[a] a; fn f f: print(f() f.string); fn g: f(1 + 2)
  many[a] -> list[a]     # ft f a: many[a] a; fn f a*: a.sum; fn g: f 1 2

- Ternary operator
  iif a >= 0 a (-1)

- Optional argument
  ft f: optional[int] int
  fn f a=1: a
  fn f a=none: a.else(1)

- Named argument
  ft f: a.int int
  fn f {a}: a # f(1); f(a=1)

- Variable argument
  ft f a: many[a] int
  fn f many[a]: a.sum
  ft g k v: many[k v] dict[k v]
  fn g many[kvs]: dict kvs

- Macro
  ft until: lazy[bool] lazy[_] _
  fn until f g: while f(): g()

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
?                  ternary operator
< <= > >= == ===   comparing
=                  update existing a variable
,                  delimiter to tuple and arguments in anonymous function
:                  delimiter to start indented block
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
  - function       # ft f a: a a; fn f a: a
  - struct         # struct a: b c
  - union          # union a: b; c d
- Statement
  - if / else      # if a: b; else if c: d; else: e
  - return         # return 1
  - match          # match a:; b: c; _: d
  - throw / catch  # fn f: throw "..."; catch f; a.t: a; b.u: b; c: c
  - for            # for i 9: n += i
  - each           # each x xs: n += x
  - while          # while a: b
  - continue       # continue
  - break          # break
  - yield          # fn f: yield 1; yield 2 # iter[int]
- Misc
  - iif            # iif a b c d e   ->   a ? b : c ? d : e
  - module         # module a: inc int int; use a inc
  - comment        # # comment
  - test           # test t "label": t.eq "hi" greet



# Advanced usage
union ast:
  real float
  op2:
    op string
    lhs ast
    rhs ast
def eval a:
  match a:
    case x.real: x
    case x.op2{op="+"}: eval(x.lhs) + eval(x.rhs)
    case x.op2{op="-"}: eval(x.lhs) - eval(x.rhs)
    else: throw $"unknown {a}"

union tree a:
  leaf
  node:
    value a
    left tree[a]
    right tree[a]
def validate t:
  match t:
    case .leaf                            : true
    case .node{value} if value == nan     : false
    case .node{value left.node right.node}: left.value <= value <= right.value && validate(left) && validate(right)
    case .node{value left.node}           : left.value <= value && validate(left)
    case .node{value right.node}          : value <= right.value && validate(right)
    # else is not needed because the above covers all
