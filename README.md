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
[1 2 3]       # array
{one 1 two 2} # dict
```

Anonymouse Function
```
a => a
a => b => a + b
```

Function
```
pi = 3
inc x = x + 1
add a.num :: a a a
add x y = x + y
```

Struct
```
person data:
  name string
  age int

dict k v data:
  values [k v]
```

ADT
```
bool adt|
  true
  false

option a adt|
  none
  some a

ast adt|
  aint int
  aop2 data:
    op string
    lhs ast
    rhs ast
```

Condition?
```
1 -> exp
tag -> exp
tag(capture) -> capture
```

Exp
```
1 + 2 * 3 == 7
```

Control Flow
```
max a b = if(a > b a b)

gcd a b = if(
  a < b  gcd(b a)
  b == 0 a
  gcd(b a/b))

show m = match(m
  none "none"
  just ++("just " m))
```

Error Handling
```
f = error("something failed")
main =
  print(f)                       # print: error(something failed\n  f:1)
  print(f.alt(1))                # print: 1
  print(f.catch(e => e.message)) # print: something failed
  e <- f                         # print: error, and exit(-1)
  print(e)                       # never reached
```

Effect
```
calc n =
  sum <- 0
  n.times(n => sum += n)
  when(sum > 100 sum := 100)
  sum
```



## 3. Syntax
```
top: func | sign | data | adt
func: id arg* "=" (line+ exp)
sign: id+ "::" type+
data: id+ ":" (indent attr)+
adt: id+ "|" (indent id (":" (indent2 attr)+)?)+

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
indent2: br "    "
```

## 4. Buildin

### Reserved
- empty
- bool
- true
- false
- int
- float
- byte
- bytes
- string
- function
- array
- dict
- opt
- some
- none
- error
- mutable

### Core data types
bool|
  true
  false
int:
  float :: float
float:
  int :: int
string:
  int   :: opt(int)
  float :: opt(float)
array a:
  size   :: int
  map b  :: (a b) [b]
  keep   :: (a bool) [a]
  dict b :: [b] dict(a b)
dict k v:
  size :: int
  get  :: opt(v)
  set  :: k v bool
opt a|
  some a
  none
  error string
  then  :: b (a b) opt(b)
  catch :: opt(a) opt(a)
  alt   :: a a

### Standard data types
byte:
  int :: int
bytes:
  array(byte)
time:
  year, month, day, hour, minute, second, yday, mday, wday :: int
  locale
date:
  year, month, day, yday, mday, wday :: int

# ideas
Monad
Eq
Default
Hash
Ord
Index

### IO

Random
```
main =
  n <- io.random.int(1 3)
  io.exit(n)
```

Time
```
main =
  now <- io.now
  io.print(now)
```





## 5. Appendix

Accepted ideas
- Generics
- Functional programming
- ADT
- Monadic error handling

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
> #                          -- comment
> ( )                        -- function call or grouping
> [ ]                        -- array
> { }                        -- map
> ->                         -- condition
> * + - / // % ^ **          -- arithmetic operators
> < > <= >= == !=            -- compare operators
> && ||                      -- boolean operators
> .                          -- property access
> :                          -- define function
> ::                         -- define prototype for function
> :::                        -- define struct?
> |                          -- define abstract data type
> =                          -- bind for monadic functions
> := += -= *= /= %= ||= &&=  -- change variable
> " ' ` $                    -- make string
> ;                          -- separator 1
> ,                          -- separator 2

- option
  ?                  -- variable arguments e.g. add x y z?0 = x+y+z

- unused
  !
  @
  ~
