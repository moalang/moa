# The Moa Programming Language
Moa is an open source programming language.
This language maximize productivity by fun programming.

Table of contents
0. Hello world
1. Value
2. Expression
3. Definition
4. IO
5. Namespace
6. Package
7. Buildin
8. Appendix
9. TODOs





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
│ └test.moa
└src
   └main.moa
```

Run
```
# moa run
Hello World.

> main.moa
- io
main = io.print("Hello World")
```

Test
```
# moa test
..x. Failed
test.moa:5|   eq(12 v)
expect: 12
  fact: 11

> test.moa
- test eq
main =
  v = 11
  eq(1 1)
  eq(12 v)
  eq(1 1)
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
(1 2)      # tuple(int int)
(x 1, y 2) # struct(x int, y int)
{1 2, 3 4} # dict(int int)
```

Closure
```
a,b => a + b # closure
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

Function
```
pi float
pi = 3.14

add int int int
add x y = x + y

sum [a] a :: a.num
sum xs = xs.reduce((+) 0)

echo =
  line <- io.readline
  io.print(line)

list.size l = l.count # [].size == [].count
```

Struct
```
vector2:
  x int
  y int
  show v = `($v.x,$v.y)`
```

Enum
```
cupon:
| none
| prize name string, price int
  show =
  | none = "none"
  | price = `$name $price`
either l r| left l | right r
```

Type class
```
addable t:
  + t t t

vector1:
  x int

addable(vector1):
  l + r = vector1(l.x + r.x)
```





## 4. IO

Standard input, output and error
```
main =
  line <- io.readline
  line.empty
  | true = io.puts(line)
  | false = io.warn(line)
```

File system
```
main =
  files <- io.fs.ls("/tmp").filter(.is_file)
  contents <- files.map(file => file.read)
```

TCP
```
main = io.tcp.listen("127.0.0.1:8080" handle):
  handle from =
    target <- from.readline
    to <- io.tcp.connect(target)
    from.bidirectional(to)
```

UDP
```
main = io.udp.bind("127.0.0.1:1234" handle):
  handle packet =
    target <- packet.string.lines.first
    io.udp.sendto(target packet)
```

Async and Cancel
```
main =
  t <- io.async(io.sleep(10))
  io.sleep(1)
  t.done?.else(t.cancel)
```

Random
```
main =
  n <- io.random.int(1 3)
  io.exit(n)
```

Time
```
main =
  now <- io.now
  io.print(now)
```





## 5. Namespace

## 6. Package
`my` is shared in this package
`our` is depend on another packages

```
# main.moa
moa v1.2
require:
  hello v1

main = io.puts(hello.hi(2))

# ~/moa/vendor/hello__v1_0_0/hello.moa
moa v1.0
define hello v1.0.0:
  hi int string

hi n = prefix + helper.suffix(n)
prefix = "hello"

# ~/moa/vendor/hello__v1_0_0/helper/suffix.moa
helper::
  suffix n = "!".repeat(n)
```



## 6. Buildin

Reserved keywards
- bool, true, false
- int, float, string
- seq, list, set, dict, tuple, struct, enum, func
- opt, do, try, io, error
- any, void
- i8, i16, i32, i64
- u8, u16, u32, u64
- f8, f16, f32, f64
- trace
- reflect
- this

Binary operators order
```
?                       # boolable
.                       # combine string or list
* // / %                # number (high)
+ -                     # number (low)
> >= < <=  == !=        # comparision (high)
|| && &                 # comparision (low)
,                       # reserved to separate struct and dictionary
:= += /= *= /= %= .= <- # effect
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
| "- " ref+                 # import
| "+ " id: (indent ref br)+ # export
| ref+ type+ (:: type+)*    # function prototype
| ref+ | (indent tag)+      # enum
| ref+ : (indent attr)+     # struct
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
| ref op2u exp # update => n += 1
| unit
unit:
| value
| id ("." id | "(" exp* ")")*
| "(" exp ")"
| "(" id+ => body ")"    # lambda : (x y => x + y)
| "(" unit (, unit)+ ")" # tuple  : (1, n)
| "(" ie (, ie)+ ")"     # struct : (name "value", age 30 + 7)
| "[" unit+ "]"          # array  : [1 2]
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
--- single ----------------------------
_    # ignore
+-/* # arithmetic
%    # mod
&    # intersection
|    # union
<>   # bool operator
=    # function
"    # string
`    # string with variables evaluation
#    # comment
,    # reserved to separete inside () and {}
.    # glue two objects
\    # escape
:    # define
?    # to boolean

--- group -----------------------------
()   # priority, tule, struct
[]   # array
{}   # dictionary

--- not decided yet -------------------
;    # glue two statements
~    # -
!    # -
$    # -
&    # -
^    # -
@    # -
'    # -
```

## 8. TODOs
- [] namespace and file structure
    public   : export(+)
    internal : ?
    private  : default
- [] compile to JavaScript
- [] self booting by compiled JavaScript
- [] make memo app
- [] dog fooding in real world
