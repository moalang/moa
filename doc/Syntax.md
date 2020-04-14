# Syntax

```
root: def (br def)*
def:
| func
| ref+ | (indent tag)+  # enum
| ref+ : (indent attr)+ # struct
| ref+ : type+          # function prototype
func: id arg* "=" body
tag : ref (attr ("," attr)*)?
attr: id type
body:
| stmt
| exp (branch | helper)?
stmt: (indent exp)+
branch: (indent "|" unit " -> " exp)* # pattern match
helper: : (indent attr)* (indent func)+
exp:
| unit op2 exp # op2 => v + 1
| ref op2u exp # set => n += 1
| unit
unit:
| value
| id ("." id | "(" exp* ")")*
| "(" exp ")"
| "(" id+ => body ")"    # lambda : (x y => x + y)
| "(" unit (, unit)+ ")" # tuple  : (1, n)
| "(" iv (, iv)+ ")"     # struct : (name "value", age 30 + 7)
| "[" unit* "]"          # array  : [1 2]
| "{" kv (, kv)+ "}"     # dict   : {"name" "value", "age" 30 + 7}
value:
| int    # 1
| float  # 1.0
| string # "hi"
| bool   # true
iv  : id exp
kv  : exp exp
id  : [a-z0-9_]+
ref : id (. id)*
type: ref ("(" ref+ ")")?
op2 : [; + - * / % & | << >> + - > >= < <=  == != || &&]
op2u: [= := += /= *= /= %=]
indent: "\n  "
br: "\n  "
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
# empty typed
array(int)
dict(string int)
# empty with capacity
array(int cap:10)
dict(string int cap:10)
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

bool|
  true
  false
option a|
  some value a
  none
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
one : opt(int)
one = ok(1)
none a : opt(a)
none = err("none")
calc : opt(int)
calc = none ||| one

run: int
run = go:
  n int
  go =
    inc
    inc
  inc : do(int)
  inc = n += 1

main : io(int)
main =
  # io.stdin.readline : io(string)
  n <- io.stdin.readline.map(to_i)
  n
```

## Name space?

```
# main.moa
use math
main = print(math.pow(math.abs(-9) 2))

# math/main.moa
in math
abs x = x > 0 | x | (-x)
pow: int int int

# math/pow.moa
pow x y = _pow(x y)
_pow x y = y <= 0
| 0
| _pow((x * x) (y - 1))
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
opt(a) # some(a) | none
do(a)  # modify memory
try(a) # opt(a) + do(a)
io(a)  # try(a) + system call
```

## Reserved type
```
i8 .. i64, u8 .. u64
f32, f64
```
