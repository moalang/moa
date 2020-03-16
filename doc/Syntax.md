# Syntax

```
root: def++
 def:
| func
| ref+ : tag* prop*
| ref+ : type+
func: ref id* "=" stmt
tag : "\n|" ref (attr ("," attr)*)*
prop: "\n" (call | attr | func)
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
args: 
| "(" id+ ")"
| id (, id)*
switch:
| (br? "|" exp){2}           # binary branch
| (br? "|" unit " : " exp)+  # pattern match
| ":\n" attr** func++
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
one: int
one = 1

inc: int int
inc x = x + one

add: int int int
add = (a b) => a + b

id a: a a
id x = x

echo: io
echo =
  line <- readline
  puts(line)
```


## Types

```
vector2:
  x int
  y int
cache k v:
  values dict(k v)

bool:
| true
| false
option a:
| some a
| none
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

## Error handling

```
div: int int try(int)
div n m =
  (m == 0) && throw "zero division error"
  n / m
calc: int
calc n m = div(n m) ||| div(m n) ||| 0
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
