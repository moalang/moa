# The Moa Programming Language
Moa is an open source programming language which helps programming for fun! 


## 1. Getting started

Set up
```
mkdir -p ~/moa/bin
curl https://github.com/moa/bin/moa > ~/moa/bin/moa
export PATH=$PATH:~/moa/bin
```

Create a code
```
# echo 'def main: io.print "hello world"' > main.moa
```

Run
```
# moa run
hello world
```

Build
```
# moa build
# ./main
hello world
```

Test
```
# echo 'test t: t.eq "hi" t.main.stdout
# moa test
expect: hi
actual: hello world\n
```

Compile to programming languages
```
# moa js
console.log("hello world")
```

```
# moa go
package main

import "fmt"

func main() {
  fmt.Println("hello world")
}
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

ADT: Algebraic data type
```
adt tree a:
  leaf a
  node:
    left tree a
    right tree a
```

Control Flow
```
def max a b:
  iif (a > b) a b

def zero n:
  if n === 0:
    io.print "zero"

def gcd a b:
  if a < b: return gcd b a
  if b == 0: return a
  gcd b a / b

def ten: for i 1...9: for j 1...9:
  print "$i x $j = ${i*j}"
  if i == 9:
    print "--\n"

def include t v:
  match t:
    leaf a: a == v
    node n: find(n.left) || find(n.right)
```

Error Handling
```
def div a b:
  if b == 0:
    error "zero division"
  a / b

def calc:
  let n div 4 2 # n should be 2
  let m div 4 0 # raise "zero division" error
  n + m         # never reached here

def main:
  for i 0 10:
    ans = catch(div(i i) e => 0)
    print i i "=" ans
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
