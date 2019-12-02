# Syntax

```
root = top+
top = package? | import* | export* | defines
package = "package" name
import = "import" name+
export = "export" id+
defines = define (BR define)*
define = func | declear
func =
| id id* "::" type+
| id arg* "=" body
field = id type value?
declear =
| "struct" id+ ":" BR? members?
| "enum" id+ ":" BR? id type? (SEP id type)*
body = closure | exp | matches | (BR INDENT (func | exp))+ with?
exp = unit (OP2 exp)?
matches = (PIPE matcher+ "=" exp)+
matcher =
| value
| id
| "[" matcher "]"
with = BR "with: " (BR INDENT (func | var))+

id = [a-zA-Z_] [a-zA-Z0-9_]
name = id ("." id)*
arg = id
type = (id | ("[" type+ "]")) "?"?
unit = bottom ("." id ("(" unit* ")")?)*
bottom = "(" (bottom | closure) ")" | value | id
member = INDENT field | func
members = member (SEP member)*
value =
| [0-9]+(. [0-9]+)*
| "true" | "false"
| '"' [^"] '"'
| "[" unit* "]"
closure = (id | (" id+ ")") "->" exp

BR = "\n"
PIPE = "\n|"
SEP = BR | ","
INDENT = "  "
OP2 = [; + - * / % & | << >> + - > >= < <=  == != || && := += /= *= /= %=]
```

## Values

```
1               # integer 64bit
1.2             # float 64bit
"string"        # string
true, false     # bool
(a,b) -> a + b  # function
```

## Containers

```
[1 2 3]          # array
(a = 1, b = "s") # struct
("a": 1, "b": 2) # dictionary
```

## Function

```
one = 1
inc x = x + one
pair x = (x = x, y = inc(x))
```


## Types

```
struct vector2 {
  x i64
  y i64
}
interface printable {
  to_string string
}
struct vector3 {
  inherit(vector2)
  inherit(printable) {
    to_string = "($x, $y, $z)"
  }
  z i64
}
enum option a {
  some a
  none
}
inc :: i64 i64
inc x = x + 1
add num(n) :: n n n # n is acceptable for i8 to i64 and f32 to f64
add x y = x + y
hello :: string io
hello name = print("Hello" name)
trace x :: x io(x)
trace x = print(x); x
```

## Sequence

```
hi = println("Hi."); println("How's going?")
reply =
  println("Oh, Hi.")
  println("Pretty good.")
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
nat n = coroutine(f -> f(n); nat(n + 1))
ten = nat(1).take(10) # [1, 2, ..., 10]
```

## Error handling

```
find a :: [a] (a bool) error(a)
find xs f = xs
| [] = error("not found")
| [y ys] = f(y) ? y : find(ys y)
match a :: [a] [a] (a a) error(bool)
match xs ys f =
  x <- find(xs f)
  y <- find(ys f)
  x == y
```


## Namespace

```
package util
import protocol
export y2j
y2j s = protocol.yaml.parse(s).(protocol.json.dump)
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
closure
coroutine
array
dict
struct
enum
error
try(a)
```
