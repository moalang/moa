# Syntax
top: line*
line: exp+ block?
block: ":" (("\n  " line)+ | exp)
exp: op1? unit (op2 exp)?
unit: bottom (prop | call | list)*
prop: "." id
call: "(" exp* ")"                           # f(a b=c)
list: "[" exp* "]"                           # a[1]
dict: "[" ":" | (unit ":" unit)+ "]"         # a["a":b c:1]
bottom:
| "(" exp ")"                     # 1 * (2 + 3)  : priority
| " ." id                         # .int         : type itself
| [0-9]+ ("." [0-9]+)?            # 1.2          : number
| '"' [^"]* '"'                   # "s"          : string
| (id ("," id)*)? "=>" exp        # a,b => a + b : lambda
| list
| id
op1: [!-]
op2:
| [+-*/%<>|&^=,]                  # (1, "hi")   -> tuple(1 "hi")
| [+-*/%<>|&^=] "="
| "**" | "&&" | "||" | ">>" | "<<"
| "===" | "**=" "<=>"
id: [a-za-z_][a-za-z0-9_]*
embedded: class union match guard error bool true false int float string bytes list set dict tuple opt some none time log test use
reserved: num



# Syntax sugger for value
| '"""' [^"]* '"""'           # """a"b"""      -> "a\"b"
| $'"' [^"]* '"'              # "s {a} {b 04}" -> "s {} {04}".format(a b)
| id ("," id)+ "=" exp        # a, b = c       -> a = c.0; b = c.1; c
| bottom "[" exp "]"          # a[1]           -> a.get(1)
| bottom "[" exp "]" "=" exp  # a[1] = 2       -> a.set(1 2)
| "0x" [0-9a-fA-F_]+          # 0xff           -> 255
| "0o" [0-7_]+                # 0o11           -> 9
| "0b" [0-1_]+                # 0b11           -> 3
| [0-9][0-9_]+ ("." [0-9_]+)? # 10_000.1_002   -> 10000.1002
| [0-9]+ or(sec second seconds min minute minutes hour hours day days month months year years) # into int



# Semantics
- Pattern match
  "match " exp ":" ("\n  " pattern (if exp)? ":" exp)+
  pattern: matcher ("," pattern)?              # a, b    : tuple
  type: ("." id prop*)+
  capture: id type?
  matcher:
  | '"' [^"]* '"'                              # "s"           : string
  | [0-9]+ ("." [0-9]+)?                       # 1.2           : number
  | "true" | "false"                           # true          : bool
  | "[" pattern* "]"                           # [0 x]         : list
  | "(" pattern ")"                            # (a, b)        : priority
  | type term?                                 # .type         : type match
  | capture term?                              # id.type       : capture and type match
  term:
  | "{" (capture ("=" exp | term)?)* "}"       # {a b.int c=1 d[0] } : match object
  | "[" (type | capture)+ "]"                  # [.int t]            : match type parameters

- Implicit type converting
  1 + 2                   # int
  1 + 2.0                 # float

- typed argument
  f a.int .int       = a     # int int
  f a b.float        = a / b # float float float
  f t.num => a.t b.t = a + b # t.num => t t t

- pass through
  f a.. = g a..

- variable length argument
  f (a=1) = a         # one or zero with default value
  f a?    = a.or(1)   # zero or one
  f a*    = a.max     # zero or more
  f a+    = a.max     # one or more
  f a,+  = a.max.1    # two, four or more : a is list[tuple[t u]]
  f                   # zero, one or two
  | = f 0
  | a.int   = f a 0
  | a.float = f a 0.0
  | a b = a + b

- named argument
  f {a}     = a    # f(a=1)
  f {a=0}   = a    # f() or f(a=1)
  f {a b}   = a    # f(a=1 b=2) or f(b=2 a=1)
  f {a b=0} = a    # f(a=1) or f(a=1 b=2) or f(b=2 a=1)

- variable
  n = 1
  n += 2
  n := 3 # ok
  n = 3  # compile error

# Symbols
_                  part of id
.                  access an element of object
"                  string
#                  comment
! -                singular operator
&& ||              binary operator for boolean
>> <<              bit shift for integer
+ - * / % **       binary operator for number
| & ^              binary operator for int(or, and, xor)
( )                priority
[ ]                list
{ }                object
< <= > >= == ===   comparing
=                  update existing a variable
,                  delimiter to tuple and arguments in anonymous function
:                  delimiter to start block
? undefined
\ undefined
@ undefined
~ undefined
' undefined
$ undefined
; undefined
` undefined # `



# Basic usage
- Primitive
  - bool      # true, false
  - int       # 1, -1
  - float     # 1.0, -1.0
  - string    # "hi"
  - lambda    # a,b => a + b
  - bytes     # bytes(1024)
- Container
  - list      # list[int](1 2)                        | list(1 2)            | [1 2]
  - tuple     # tuple[int string](1 "hi")             | tuple(1 "hi")        | 1, "hi"
  - set       # set[int](1 2)                         | set(1 2)
  - object    # object[a.int b.(int int)](a x => g x) | object: a; b=1; f x = g x | {a; b=1; f(x)=g(x)}
  - union     # union t: a; b int; c: d int
  - class     # class t: a int; b t
- Definition
  - variable  # a = 1
  - function  # f a b = a + b    | f(a b) = a + b
  - property  # now() = io.now
- Branch
  - match     # match a:; b: c; b: d
  - guard     # guard a b
  - error     # match error("failed" 1); a.error[int]: a.value.string ++ a.stack; b.error: b.message; c: c
- Misc
  - comment   # # comment
  - test      # test t: t.eq "hi" greet



# Advanced usage
ast = union:
  real float
  op2:
    op string
    lhs ast
    rhs ast
eval node = match node:
  x.real: x
  x{op="+"}: eval(x.lhs) + eval(x.rhs)
  x{op="-"}: eval(x.lhs) - eval(x.rhs)
  x: throw $"unknown {x}"

tree a = union:
  leaf
  node:
    value a
    left tree[a]
    right tree[a]
validate t = match t:
  .leaf                  : true
  {left.leaf right.leaf} : true
  n{left.node right.node}: left.value <= n.value <= right.value && validate(left) && validate(right)
  n{left.node}           : left.value <= n.value && validate(left)
  n{right.node}          : n.value <= right.value && validate(right)
  # else is not needed because the above covers all
