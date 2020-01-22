# Syntax

```
root: def++
 def:
| ref id* "=" statement      # function or assign
| ref id* ":" type           # function signature
| ref id* ":" (indent prop)+ # type defintion
| ref id* ":|" (indent tag)+ # enum definition
 tag: ref (prop ("," prop)*)*
prop: attr | func
attr: id type
func: id id* ":" statement
statement:
| line
| (indent line)+
line: exp (; exp)* switch?
exp:
| unit ("," unit)+ # tuple  : 1, 2
| unit op2 exp     # op2    : v + 1
| ref op2u exp     # set    : n = 0, n += 1, n := -1
| args => exp      # lambda : x, y => x + y
| unit
unit:
| value
| call
| "[" unit* "]" # array : [1]
| "(" id exp ("," id exp)+ ")" # struct?
| "(" id ":" exp ("," id ":" exp)+ ")" # dictionary?
| "(" exp "=" exp ("," exp "=" exp)+ ")" # dictionary?
| "(" func | exp ")"
value:
| int    # 1
| float  # 1.0
| string # "hi"
| bool   # true
  id: [a-z0-9_]+
 ref: id (. id)*
type: id call*
call: id ("." id | "(" exp* ")")*
 op2: [; + - * / % & | << >> + - > >= < <=  == != || &&]
op2u: [= := += /= *= /= %=]
args: "()" | id (, id)*
indent: "\n  "
switch:
| (br "|" exp){2}           # binary branch
| (br "|" unit " : " exp)+  # pattern match
indent: "  "+
```

## Values

```
1               # integer 64bit
1.2             # float 64bit
"string"        # string
true            # bool
(a,b) => a + b  # function
```

## Containers

```
[1 2 3]        # array
(a: 1, b: "s") # struct?
(n = 1, 2 = 2) # dictionary?
```

## Function

```
one: 1
inc x: x + one
int.neg n: n * -1
```


## Types

```
vector2: x int, y int
printable: to_string (string)
vector3: +vector2 +printable(to_string: "($x, $y, $z)") z int
option a: some a | none
functions:
  inc :: i64 i64
  inc x: x + 1
  add n.num :: n n n # n is acceptable for i8 to i64 and f32 to f64
  add x y: x + y
  hello :: string void
  hello name: print("Hello" name)
  trace printable(x) :: x x
  trace x: print(x); x
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

## Coroutine

```
nat n = coroutine(n, () => nat(n + 1))
ten = nat(1).take(10) # [1, 2, ..., 10]
```

## Error handling

```
find any(a) [a] (a bool) error(a)
find xs f: xs
| [] = error("not found")
| [y ys] = f(y) ? y : find(ys y)
match any(a) [a] [a] (a a) error(bool)
match xs ys f:
  x = find(xs f)
  y = find(ys f)
  x == y
```


## Order of operation

Binary operations

```
* / %
& | << >>
+ -
> >= < <=  == !=
|| &&
:= += /= *= /= %=
```

## Reserved keyword

```
true, false
```

## Core types

```
bool
int
float
string
function
coroutine
array
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
