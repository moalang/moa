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
true # bool
1    # int
1.0  # float
"hi" # string
```

Container
```
a,b     # tuple
[1]     # array
a:1,b:2 # struct
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

Struct
```
person: # ":" + "\n" define struct
  name string
  age int

dict k v:
  values [k,v]
```

ADT
```
bool| # "|" + "\n" define algebraic data type
  true
  false

result a|
  success a
  failure:
    message string
```

Type class
```
.eq t:
  eq :: t t bool
  eq a b = a == b

.int t:
  (+,-,*) :: t t t

.num t.int:
  (/) :: t t t
  (//) :: t t int
  (//) l r = int(l / r)

.monad t.(_ => _):
  return a :: a t(a)
  bind a b :: t(a) (a t(b)) t(b)
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
  just v => "just " . v)
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
top: func | sign | data | type | adt
func: id+ "=" (line+ | exp)
sign: id+ "::" type+
data: id+ ":" (indent attr)+
type: "." id+ ":" (indent attr)+
adt: id+ "|" (indent id body?)+
body: id | ":" (indent2 attr)+

line: indent exp
exp: unit (op2 exp)*
unit:
| value (call | prop)*
value:
| "(" unit ")"
| "[" exp* "]" # array
| kv ("," kv)* # struct
| num ("." num)?
| '"' [^"] '"'
| args "=>" exp
| id
call: "(" exp+ ")"
prop: "." id
kv: id ":" exp
op2: + - * / // % = += -= *= /= == != || && >= > <= <
args: id ("," id)*

id: [A-Za-z_][A-Za-z0-9_]
num: [0-9]+
attr: sign | id value
type: "$" ? texp (, texp)* # "$" means mutable
texp: id | "[" type "]" | "(" type type+ ")"
indent: "\n  "
indent2: "\n    "
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
- array    : [1 2]
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
Monad   # => error monad?
Eq      # .eq t: eq :: t t bool
Default
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
