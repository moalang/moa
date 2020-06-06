# The Moa Programming Language
Moa is an open source programming language.
This language maximize productivity by fun programming.

Table of contents
1. Value
2. Expression
3. Definition
4. IO
5. Debugger
6. Test
7. Package
8. Buildin
9. Appendix





## 1. Value

Primitive
```
true # bool
1    # int 64bit
1.0  # float 64bit
"hi" # string utf8
```

Container
```
[1 2]             # list
(1, 2)            # tuple
(x 1, y 2)        # struct
{1 true, 2 false} # dict
```

Closure
```
x => x # closure
```





## 2. Expression

Binary operator
```
1 + 2 * 3 # 7
```

Branch
```
a > b
| true = 1
| false = 0
```

Sequence
```
value <- calculate
value + 1
```





## 3. Definition

Function
```
pi = 3.14
add x y = x + y

id a: a a
id x = x

dice =
  r <= random(10)
  r % 2 == 0
  | true = "even"
  | false = "odd"
```

Struct
```
vector2:
  x int
  y int
```

Enum
```
maybe a:
| some a
| none
```





## 4. IO
File system
```
TBD
```

TCP
```
TBD
```

UDP
```
TBD
```

Async and Cancel?
```
use io async
main =
  [1,2,3].each(x => async(() => sleep(x)))
  async.wait(2.seconds)
  | async.cancel
```





## 5. Debugger
```
moa debug ...
exit -127
> show lines
main.moa:24 | main = run("...")
main.moa:54 | run arg =
main.moa:66 |   listen(arg)
main.moa:94 | listen ip_port =
...
```






## 6. Test
```
moa test [name ...]
pass file name
pass file name
...
pass: 200
failed: 3
time: sum 24s, min 1ms, max 9ms, avg 12ms, mid 100ms
```





## 7. Package
Use package
```
use io.tcp             # tcp.listen
use io.file open close # open("path" .rw)
use hello.v1           # hello.world(2)
```

Define package
```
package hello
def world int io
world n = "hello world" + "!".repeat(n)
```





## 8. Buildin

Reserved keywards
- bool, true, false
- int, float, string
- seq, list, dict, tuple, struct, enum, func
- opt, do, try, io, error
- any, void
- i8, i16, i32, i64
- u8, u16, u32, u64
- f8, f16, f32, f64
- type?, array?

Binary operators
```
++                     # list?
.                      # string?
* // / %               # number (high)
+ -                    # number (low)
> >= < <=  == !=       # comparision (high)
|| &&                  # comparision (low)
:= += /= *= /= %= <-   # effect
```

Expression
```
|                      # gard
,                      # separator
```

Unused
```
@ & &&& |||
```

Reserved
```
! ? ^
```




## 9. Appendix

Design of Moa language
- List bounds check
- Linear logic resource management
- Threads and STM
- Preconditions and Postconditions

Pending ideas
- Global variables
- Pointer
- Weak reference
- Using up many core
- Asynchronous IO
- Watch dog timer
- Shared nothing for concurrency
- Logger, debugger, profiler and resources monitor
- Strong composability

Rejected ideas
- Null
- Exception
- Shadowing
- Default mutable
- Allocate / free
- Macro
- Class
- Interface
- Out of range access
- Zero division
- Standard library
- GC
- Globalization
- Type level programming

Syntax
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

All single symbols

```
--- decided ----------------------------
_    # namable
+-/* # arithmetic
%    # mod
<>   # bool operator
()   # priority, tule, struct
[]   # array
=    # function
"    # string
#    # comment
,    # separator inside (), {}
.    # call
\    # escape

--- provisional ------------------------
;    # glue?
:    # define?
{}   # dictionary?

--- not decided yet -------------------
~    # -
|    # -
!    # -
$    # -
&    # -
?    # -
^    # -
@    # -
'    # -
`    # -
```

TODO (v0.1 self booting)
[x] Design error handling type and sequence syntax
[x] Tidy up documents
[x] Uniqueness
    1. pure
    2. readable short code
    3. it compiles to other languages
[x] Error handling design
   # pure
   f : int, f : pure(int)
   f = 1
   # mutable
   f : do(int)
   f = var += 1
   # failable
   f : opt(int)
   f = err("failed").or(1).then(x => x + 1)
   # try = mutable + failable
   f : try(int)
   f = var += 1; ok(var)
   # io = system call
   f : io(int)
   f = io.stdin.readline..to_int
   # panic = unrecoverable errors
   f : array(int)
   f = int[int.max] # panic("out of memory")
   f = panic("unreachable")
[x] Minimal compiler to Ruby
[x] Self booting with Ruby

# TODO (v0.2 JavaScript)
[x] Minimal compiler to JavaScript
[] Feedback to syntax from making minimal compiler to JavaScript
[] Making API server by nodejs
[] Making Kakeibo app on GAE

# TODO (v0.4 enhance built-in functions)

# TODO (v0.5 tracer)

# TODO (v1.0 relace)
