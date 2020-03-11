# Syntax

```
root: def++
 def:
| func
| ref+ tag++
| ref+ : prop++
| ref+ :: type+
func: ref id* "=" stmt
tag : "|" ref (prop ("," prop)*)*
prop: call | attr | func
call: id ("." id | "(" exp* ")")*
attr: id type
stmt: exp switch?
exp:
| unit op2 exp # op2    : v + 1
| ref op2u exp # set    : n = 0, n += 1, n := -1
| args => exp  # lambda : x, y => x + y
| unit
unit:
| value
| call
| "(" unit+++ ")" # tuple  : (1, n)
| "(" kv** ")"    # struct : (name "value")
| "[" unit** "]"  # array  : [1]
| "{" kv** "}"    # dict   : {"key" "value"}
| "(" func | exp ")"
value:
| int    # 1
| float  # 1.0
| string # "hi"
| bool   # true
kv  : id exp # decided?
id  : [a-z0-9_]+
ref : id (. id)*
type: ref | "(" ref+ ")"
op2 : [; + - * / % & | << >> + - > >= < <=  == != || &&]
op2u: [= := += /= *= /= %=]
args: "()" | id (, id)*
switch:
| (br? "|" exp){2}           # binary branch
| (br? "|" unit " : " exp)+  # pattern match
```

## Primitives

```
1    # integer 64bit
1.2  # float 64bit
"hi" # string
true # bool
```

## Containers

```
# empty
()           # struct
[]           # array
{}           # dictionary
# single
(a 1)        # struct
[1]          # array
{"a" 1}      # dictionary
# multiple
(1, n)       # tuple
(a 1, b n)   # struct
[1 n]        # array
{"a" 1, k n} # dictionary
```

## Function

```
one = 1
inc x = x + one
add = (a b) => a + b
id x = x
echo =
  line <- readline
  puts(line)
```


## Types

```
printable:
  string: string
vector2:
  x int
  y int
  printable("($x,$y)")
cache k v:
  values dict(k v)

bool:
| true
| false
option a:
| some a
| none

inc: int int
inc x = x + 1

add n.num: n n n
add x y = x + y

id x: x x
id x = x

echo =
  line <- readline
  puts(line)
ast:
  int int
  op2:
    op2 string
    left ast
    right ast
parse src = top:
  pos int
  top = or(exp number)
  or l r = l | _ -> r
  exp =
    l <- number
    or(op2(l) l)
  op2 l =
    op <- operator
    r <- exp
    ast.op2(op l r)
  number =
    n <- many(satisfy("0123456789".contains))
    ast.int(n)
  many f = rec([]):
    rec acc =
      v <- f
      | _ -> return acc
      rec(acc ++ v)
  satisfy f =
    c <- src(pos)
    guard f(c)
    pos += 1
    c
```

## Branch

```
gcd a b = b == 0
| a
| gcd(b a % b)
fib x =
| (< 0) = 0
| 1 = one
| _ = fib(x) + fib(x - 1)
```

## Sequence

```
nat n = seq(n () => nat(n + 1))
ten = nat(1).take(10) # [1, 2, ..., 10]
```

## Error handling

```
find a: seq(a) (a bool) try(a)
find s f =
  v <- s
  f(v)
  | v
  | find(s f)
```

## Name scope

```
library foo.bar {
  add: int int int
  person:
    name string
    age int
}
belong foo.bar
use net.tcp, protocol.http
```

## Order of operation

Binary operations

```
* / %
<< >>
+ -
> >= < <=  == !=
|| &&
:= += /= *= /= %=
```

## Reserved keyword

```
true, false
__... # __ perfix is reserved
```

## Core types

```
type
bool
int # i64
float # f64
string
func
seq
array
tuple
dict
struct
enum
error
try(a)
```

## Reserved type
```
i8 .. i64, u8 .. u64
f32, f64
```
