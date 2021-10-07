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
def add a b: a + b
```

Test
```
> moa test
.x Failed
test.moa:3|  t.eq 2 add(1 2)
expect: 2
  fact: 3

# test.moa
test t:
  t.eq 2 (add 1 1)
  t.eq 2 (add 1 2)
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
{x y} # struct
```

Anonymouse Function
```
a,b => a + b
```

Function
```
def inc x: x + 1
def add x y: x + y
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

Control Flow
```
def max a b: if(a > b a b)

def gcd a b: if:
  a < b  (gcd b a)
  b == 0 a
  _      (gcd b a/b)

def show m: match m:
  none      "none"
  just v => "just " ++ v

def ten: for i 1...9: for j 1...9:
  print "$i x $j = ${i*j}"
  if i == 9:
    print "--\n"
```

Error Handling
```
def div a b: (if b == 0
  (error "zero division")
  a / b)

def main:
  let n (div 4 2) # n should be 2
  let m (div 4 0) # right side expression should be failed by error
  n + m           # never reached here
```

Variable
```
def main:
  var a 1
  a += 2 # a will be 3
  def inc: a += 1
  def add n: a += n
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
op2: + - * / // % = += -= *= /= == != || && >= > <= <
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
def main:
  let now io.now
  io.print now

  let input io.read
  io.print input

  n <- (io.random.int 1 3)
  io.exit n
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
> = += -= *= /= %= ||= &&=   -- change variable
> " ' ` $                    -- make string
> ,                          -- tuple
> [ ]                        -- array
> { }                        -- struct

- option
> ?                          -- variable arguments e.g. add x y z?0 = x+y+z
> ;                          -- separator?
> ->                         -- condition?

- unused
> !                          -- unwrap?
> @
> ~
> :
> |
> &
