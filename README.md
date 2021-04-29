# The Moa Programming Language
Moa is an open source programming language.


## 1. Getting started

Set up
```
mkdir -p ~/moa/bin
curl https://github.com/moa/releases/mac/latest > ~/moa/bin/moa
export PATH=$PATH:~/moa/bin
```

Make a project
```
# moa new
├README.md
├test
│ └test.moa
└src
   └main.moa
```

Run
```
> moa js
  function add(a, b) {
    return a + b
  }
  export({ add })


> moa go
  package main
  func add(a int, b int) int {
    return a + b
  }


# main.moa
add a b = a + b
```

Test
```
> moa test
.x Failed
test.moa:5|  eq(2 add(2 2))
expect: 2
  fact: 4

# test.moa
test t =
  t.eq(2 add(1 1))
  t.eq(2 add(2 2))
```





## 2. Basic

Primitives
```
true       # bool
1          # int 64bit signed integer
1.0        # float 64bit
"hi"       # string utf8
`Hi $name` # $.. or ${..} will be evaluate as string
```

Container
```
[1 2 3]       # list
{1 2 3}       # set
{one:1 two:2} # dict
```

Anonymouse Function
```
a => a
(a b) => a + b
```

Types
```
person:
  name string
  age int

dict k v:
  values [k v]

bool|
  true
  false

maybe a|
  none
  just a

ast|
  aint int
  aop2:
    op string
    lhs ast
    rhs ast
```

Function
```
pi = 3
inc x = x + 1
add x y = x + y
```

Exp
```
1 + 2 + 3        == (+ 1 2 3)
"a" ++ "b ++ "c" == (++ "a" "b" "c")
```

Control Flow
```
max a b = if(a > b a b)

show m = match(m
  none "none"
  just v => ++("just " v))

gcd a b = if(
  a < b gcd(b a)
  b ==  0 a
  gcd(b a/b))
```

Error Handling
```
f = error("something failed")
main =
  r = f
  print(r)                       # print: error(something failed\n  f:1)
  print(r.alt(1))                # print: 1
  print(r.catch(e => e.message)) # print: something failed
  e <- f                         # print: error, and exit(-1)
  print(e)                       # never reached
```

Effect
```
token:
  tag string
  code string
  pos int
tokenize :: string may([token])
tokenize src =
  pos <- var(int)
  satisfy f =
    p <- pos
    c <- src.at(p)
    guard(f(c))
    c
  many f = (g acc = g.then(x => g(acc.push(x))).alt(acc))([])
  many1 f =
    x <- f
    xs <- many(f)
    [x] ++ xs
  read_tag t f =
    chars <- many1(satisfy(f))
    p <- pos
    token(t chars.join("") p)
  read_id = read_tag("id" c => "a" <= c <= "z")
  read_num = read_tag("num" c => "0" <= c <= "9")
  many1(raed_id.alt(read_num))
```



## 3. Syntax
```
top: define | export | import
define: func | sign | data | adt
func: id arg* "=" (line+ exp)
sign: id+ "::" type+
data: id+ ":" (br attr)+
adt: id+ "|" (br id attr*)+
export: "- " id ":" (indent define)+
import: "- " id+

arg:
| id [*+?]?
| "[" arg* "]"
line: indent body ("if" exp)?
body:
| var id type
| exp
exp: unit (op2 exp)?
unit:
| "(" exp+ ")" call?
| "[" exp* "]"
| num ("." num)?
| '"' [^"] '"'
| id call?
call: "(" exp+ ")"
op2: + - * / // % = += -= *= /= == != || && >= > <= < =>

id: [A-Za-z_][A-Za-z0-9_]
num: [0-9]+
br: "\n"
attr: id type
type: id | "[" type (":" type)? "]" | "(" type type+ ")"
indent: br "  "
```

## 4. Buildin

### Reserved
- any bool true false int float byte bytes string list dict may some error var

### Core data types
bool|
  true
  false
int:
  neg :: int
  abs :: int
  float :: float
float:
  neg :: float
  abs :: float
  int :: int
byte:
  int :: int
bytes:
  list(byte)
string:
  int   :: may(int)
  float :: may(float)
tuple a+:
  ...
list a:
  size   :: int
  map b  :: (a b) [b]
  keep   :: (a bool) [a]
  dict b :: [b] dict(a b)
dict k v:
  size :: int
  get  :: may(v)
  set  :: k v bool
may a|
  some a
  error string
  then b :: (a b) may(b)
  catch  :: may(a) may(a)
  alt    :: a a
# ideas
Monad
Eq
Default
Hash
Ord
Index

### Binary operators order
```
* // / %         # number (high)
+ -              # number (low)
> >= < <=  == != # comparision (high)
|| && | &        # comparision (low)
:= :             # define
+= -= *= /= %= = # effect
```

### IO

Random
```
- io
main =
  n <- io.random(1 3)
  io.exit(n)
```

Time
```
- io
main =
  now <- io.now
  io.print(now)
```





## 5. Appendix

Accepted ideas
- Generics
- Functional programming
- ADT
- Exception
- Macro for only special form

Pending ideas
- Global variables
- Pointer
- Weak reference
- Logger, debugger, profiler and resources monitor
- Strong composability
- Preconditions and Postconditions

Rejected ideas
- Monad
- Null
- Shadowing
- Default mutable
- Allocate / free
- Class
- Interface
- Out of range access
- Zero division
- Standard library
- GC
- Destructor
- Finalizer
- Globalization
- Type level programming

Symbols
- used
  #                -- comment
  ( )              -- function call or grouping
  [ ]              -- list
  * + - / % ^ ++   -- arithmetic operators
  < > <= >= == !=  -- compare operators
  && ||            -- boolean operators
  &&& |||          -- bit operators
  .                -- property reference
  :                -- define function
  ::               -- define struct, interface and function prototype
  :|               -- define ADT
  =                -- set evaluated constant
  :=               -- set evaluated variable
  <-               -- override variable >
  _                -- part of id
  " \ ` $          -- string `

- maybe
  { }              -- map or statement?
  ?                -- variable arguments e.g. add x y z?0 = x+y+z

- unused
  !
  '
  ,
  ;
  @
  ~
