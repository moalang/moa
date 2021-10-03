# The Moa Programming Language
Moa is an open source programming language which helps programming for fun! 


## 1. Getting started

Set up
```
mkdir -p ~/moa/bin
curl https://github.com/moa/bin/moa > ~/moa/bin/moa
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
test.moa:3|  t.eq 2 add(1 2)
expect: 2
  fact: 3

# test.moa
test t =
  t.eq 2 add(1 1)
  t.eq 2 add(1 2)
```





## 2. Basic

Primitives
```
true # bool
1    # int
1.0  # float
"hi" # string
```

Containers
```
(a,b) # tuple
[1 2] # array
{x y} # data
```

Anonymouse Function
```
a,b => a + b
```

Function
```
inc :: int int # signature is optional
inc x = x + 1
add a.num :: a a a
add x y = x + y
```

Exp
```
1 + 2 * 3 == 7
```

Struct
```
struct person:
  name string
  age int

struct dict k v:
  values [k,v]
```

ADT
```
adt bool:
  true
  false

adt tree a:
  leaf a
  node tree(a) tree(a)
```

Interface
```
interface eq t:
  eq :: t t bool
  eq a b = a == b

struct vector2:
  x int
  y int

extend vector2 eq:
  eq l r = l.x == r.x && l.y == r.y
```

Control Flow
```
max a b = if(a > b a b)

gcd a b = if:
  a < b  -> gcd(b a)
  b == 0 -> a
  _      -> gcd(b a/b)

show m = match m:
  none   -> "none"
  just v -> "just " ++ v

ten = for i 1...9: for j 1...9:
  print "$i x $j = ${i*j}"
  if i == 9:
    print "--\n"
```

Error Handling
```
div :: int int try(int)
div a b = if(
  b == 0 error("zero division")
  a / b)

main :: try(int)
main =
  n <- div(4 2) # n should be 2
  m <- div(4 0) # right side expression should be failed by error
  n + m         # never reached here
```

Variable
```
main =
  a <- 1
  a += 2 # a will be 3
  inc = a += 1
  add n = a += n
  inc    # a will be 4
  add(3) # a will be 7
```


## 3. Syntax
```
top: exp+
exp: atom (op2 exp)*
atom:
| ":" block
| value suffix*
suffix:
| "(" top ")"
| "." id
value:
| id
| num ("." num)?
| "(" top ")"
| "[" top? "]"          # array
| "{" (id ":" exp)* "}" # struct
| " [^"]* "             # string
| ` [^`]* `             # dynamic string
block:
| (indent top)+
| exp

id: [A-Za-z_][A-Za-z0-9_]
num: [0-9]+
op2: + - * / // % = += -= *= /= == != || && >= > <= < ->
indent: "\n" " "+
```

## 4. Buildin

### Embedded primitives
- bool     : true, false
- int      : 0
- float    : 0.0
- string   : "hello"
- function : a => a
- struct   : a:1

### Embedded containers
- array : [1 2]
- dict
- byte, bytes
- opt, ok, none, error
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
Monad # => error monad?
Eq    # .eq t: eq :: t t bool
Hash
Ord
Index

### IO

```
main =
  now <- io.now
  io.write(now)

  input <- io.read
  io.write(input)

  n <- io.random.int(1 3)
  io.exit(n)
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
> * + - / // % ^ **          -- arithmetic operators
> < > <= >= == !=            -- compare operators
> && ||                      -- boolean operators
> .                          -- property access
> =                          -- define function
> :                          -- define struct
> |                          -- define abstract data type
> ::                         -- define signature for function
> := += -= *= /= %= ||= &&=  -- change variable
> " ' ` $                    -- make string
> ,                          -- tuple

- option
> ?                  -- variable arguments e.g. add x y z?0 = x+y+z
> ;                          -- separator 1?
> ->                         -- condition?
> [ ]                        -- array?
> { }                        -- map?

- unused
> !                          -- unwrap?
> @
> ~
