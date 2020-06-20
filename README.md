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
├test
│ └test_main.moa
└src
   └main.moa
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
[1 2]      # list(int)
(1, 2)     # tuple(int int)
(x 1, y 2) # struct(x int, y int)
{1, 2}     # set(int)
{1 2, 3 4} # dict(int int)
```

Closure
```
x => x # closure
```





## 2. Expression

Binary operation
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
  show v = `($v.x,$v.y)`

dict k v: values [(k, v)]
```

Enum
```
cupon:
| none
| prize name string
  show c =
  | none = "none"
  | prize = c.name
maybe a| just a | nothing
```

Function
```
pi float
pi = 3.14

add int int int
add x y = x + y

sum [a] a :: a.num
sum xs = xs.reduce((+) 0)

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

Type class
```
addable t: + t t t
readable t: show string
vector1: x int
vector1.addable: + v = vector1(x + v.x)
vector1.readable: show = x.string
```





## 4. IO

Standard input, output and error
```
- io.std readline puts warn 
main =
  line <- readline
  line.empty
  | true = puts(line)
  | false = warn(line)
```

File system
```
- io fs
main =
  files <- fs.ls("/tmp").filter(.is_file)
  contents <- files.map(path => fs.open(path .r f => f.read))
```

TCP
```
- io.tcp listen connect
main =
  listen("127.0.0.1:8080" from =>
    target <- from.readline
    to <- connect(target)
    from.bidirectional(to))
```

UDP
```
- io.udp bind sendto
main =
  bind("127.0.0.1:1234" packet =>
    target <- packet.string.lines.first
    sendto(target packet))
```

Async and Cancel?
```
- io
main =
  [1,2,3].each(x => io.async(() => io.sleep(x)))
  wait(2.seconds)
  | io.cancel
  # here, automatically call wait
```

Random
```
- io
main =
  n <- io.ranodm.int(0 2)
  io.exit(n)
```

Time
```
- io
main =
  now <- io.time.now
  io.print(now)
```





## 5. Package
Define package
```
- hello:
  hi int string
  vector1: x int

hi n = message + "!".repeat(n)
show v = v.string
message = "hi" # private function
```

Use package
```
- io print
- hello hi vector1
main = print(vector1(1).show)
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
++                   # list?
.                    # string?
* // / %             # number (high)
+ -                  # number (low)
> >= < <=  == !=     # comparision (high)
|| &&                # comparision (low)
:= += /= *= /= %= <- # effect
```

Expression
```
|                    # gard
,                    # separator
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
| "- " ref+             # import
| "- " id:              # export
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
| "(" ie (, ie)+ ")"     # struct : (name "value", age 30 + 7)
| "[" unit+ "]"          # array  : [1 2]
| "{" unit+ "}"          # set    : {1 2}
| "{" ee (, ee)* "}"     # dict   : {"name" "value", "age" 30 + 7}
value:
| int    # 1
| float  # 1.0
| string # "hi"
| bool   # true
ie  : id exp
ee  : exp exp
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
