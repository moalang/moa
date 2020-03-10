# Syntax

```
root: def++
 def:
| func
| "class" ref+ : prop++
| "enum"  ref+ : tag++
| "alias" ref+ : ref
| "func"  ref+ : ref+
func: ref id* "=" stmt
tag : ref (prop ("," prop)*)*
prop: attr | func
attr: id type
stmt: exp switch?
exp:
| unit op2 exp     # op2    : v + 1
| ref op2u exp     # set    : n = 0, n += 1, n := -1
| args => exp      # lambda : x, y => x + y
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
type: ref
call: id ("." id | "(" exp* ")")*
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
```


## Types

```
class error: message string, backtrace array(string,int)
class tuple: values array(type)
class cache k v: values dict(k v)

enum bool: true | false
enum optionx a: some a | none
enum try a: ok a | na error

alias age: int

interface printable: string string
interface mappable a b: map seq(a) (a b) seq(b)

func inc: int int
func add: n.num: n n n
func id x: x x
inc x = x + 1
add x y = x + y
id x = x
echo =
  line <- readline
  puts(line)
parse = or(exp number)
  exp = ...
  number = ...
```

## Branch

```
gcd a b = b == 0 | a | gcd(b a % b)
fib x =
| (< 0) = 0
| 1 = one
| _ = fib(x) + fib(x - 1)
```

## Sequence

```
nat n = seq(n, () => nat(n + 1))
ten = nat(1).take(10) # [1, 2, ..., 10]
```

## Error handling

```
func find a: seq(a) (a bool) try(a)
find s f =
  v <- s
  f(v) | v | find(s f)
func find_pair a: seq(a) seq(a) (a bool) try(bool)
find_pair xs ys f =
  x <- find(xs f)
  y <- find(ys f)
  x == y || find_pair(xs ys f)
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
