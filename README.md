# The Moa Programming Language
Moa is an open source programming language.
This language maximize productivity by fun programming.

Table of contents
0. Hello world
1. Value
2. Expression
3. Definition
4. IO
5. Package
6. Buildin
7. Appendix





## 0. Hello world

Set up
```
mkdir -p ~/moa/bin
curl https://github.com/moa/releases/mac/latest > ~/moa/bin/moa
export PATH=$PATH:~/moa/bin
```

Make a project
```
# moa new
├── test
│   └── test_main.moa
└── src
    └── main.moa
```

Run
```
# moa run
Hello World.
```

Test
```
# moa test
...x. Failed
filename.moa:88|   eq(12 answer)
expect: 12
  fact: 11
```




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
n
| 0 = "zero"
| 1 = "one"
| _ = "many"

a > b
| "true"
| "false"
```

Sequence
```
value <- calculate
value + 1
```





## 3. Definition

Struct
```
vector2:
  x int
  y int
```

Enum
```
bool:
| true
| false
```

Function
```
pi: float
pi = 3.14

add: int int int
add x y = x + y

id a: a a
id x = x

console = loop:
  loop =
    print("> ")
    readline
    | "exit" = void
    | line = execute(line)
  execute cmd =
    print(cmd)
    loop
```





## 4. IO

Standard input, output and error
```
use io stdin stdout stderr
main =
  line <- stdin.readline
  line.empty
  | true = stdout(line)
  | false = stderr(line)
```

File system
```
use io.fs open ls
main =
  files <- ls("/tmp").filter(.is_file)
  contents <- files.map(path => open(path .r f => f.read))
```

TCP
```
use tcp listen connect
main =
  listen("127.0.0.1:8080" from =>
    target <- from.readline
    to <- connect(target)
    from.bidirectional(to))
```

UDP
```
use udp bind sendto
main =
  bind("127.0.0.1:1234" packet =>
    target <- packet.string.lines.first
    sendot(target packet))
```

Async and Cancel?
```
use io async wait cancel sleep
main =
  [1,2,3].each(x => async(() => sleep(x)))
  wait(2.seconds)
  | cancel
  # here, automatically call wait
```

Random
```
use io random exit
main =
  n <- ranodm.int(0 2)
  exit(n)
```

Time
```
use io time
main =
  now <- time.now
  io.print(now)
```





## 5. Package
Use package
```
use io.tcp             # tcp.listen
use io.file open close # open("path" .rw)
use hello              # hello.world(2)
use hello int.double   # 3.double
use hello int.*        # 3.double
```

Define package
```
package hello

world: int string
world n = "hello world" + "!".repeat(n)

double: int int
double i = i * 2
```





## 6. Buildin

Reserved keywards
- bool, true, false
- int, float, string
- seq, list, dict, tuple, struct, enum, func
- opt, do, try, io, error
- any, void
- i8, i16, i32, i64
- u8, u16, u32, u64
- f8, f16, f32, f64
- trace
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




## 7. Appendix

Design of Moa language
- List bounds check
- Linear logic resource management
- Threads and STM

Pending ideas
- Global variables
- Pointer
- Weak reference
- Using up many core
- Asynchronous IO
- Watch dog timer
- Logger, debugger, profiler and resources monitor
- Strong composability
- Preconditions and Postconditions

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
