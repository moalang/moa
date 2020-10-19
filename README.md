# The Moa Programming Language
Moa is an open source programming language.
This language maximize productivity by fun programming.

Table of contents
0. Hello world
1. Value
2. Expression
3. Definition
4. Namespace
5. Buildin
6. Appendix
7. TODOs





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
> moa run
Hello World.

# main.moa
main = io.print("Hello World")
```

Test
```
> moa test
..x. Failed
test.moa:5|  eq(12 v)
expect: 12
  fact: 11

# test.moa
- test: t
main =
  v = 11
  t.eq(1 1)
  t.eq(12 v)
  t.eq(1 1)
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
1, 2       # tuple(int int)
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
1 + 2 * 3 # = 7
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

statement
| v -> "succeed $v"
| e -> "failed $e"
```

Statement
```
value <- calculate(1)
func
```





## 3. Definition

Function
```
pi : float
pi = 3.14

id a : a a
id x = x

add : int int int
add x y = x + y

sum a.num : [a] a
sum xs = xs.reduce((+) 0)

echo =
  line <- io.readline
  io.print(line)
```

Struct
```
vector2:
  x int
  y int
  show v = `($v.x,$v.y)`

dict k v: kvs [k, v]
```

Enum
```
cupon:
| none
| prize name string, price int
  show =
  | none = "none"
  | prize = `$name $price`

either l r: | left l | right r
```

Type class
```
addable t:
  + t t t

vector1:
  x int

vector1.addable:
  + l r = vector1(l.x + r.x)
```





## 4. Namespace

Define name space
```
- math
pi = _private_pi
_privaet_pi = 3.141592653589793
```

Use name space
```
- main: io math

main = io.print(math.pi)
```

Refer file
```
# logger.moa
- logger: io

logger::
  debug x = io.print(x)
```

```
# main.moa
- main: logger

main = logger.debug("hello world")
```


## 5. Buildin

Reserved words
types
- bool, true, false
- int, float, string
- seq, list, set, dict, tuple, func
- opt, nil, do, try, error
- any, void
- i8, i16, i32, i64
- u8, u16, u32, u64
- f8, f16, f32, f64
methods
- trace
values
- this
- io, eff

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

### IO

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
main = io.tcp.listen("127.0.0.1:8080" _handle):
_handle from =
  target <- from.readline
  to <- io.tcp.connect(target)
  from.bidirectional(to)
```

UDP
```
main = io.udp.bind("127.0.0.1:1234" _handle):
_handle packet =
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








## 6. Appendix

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
| "- " ref (: ref+)?     # namespace
| ref+ (" : " type+)?    # signature
| ref+ | (indent tag)+   # enum
| ref+ : (indent attr)+  # struct
| func
func: id arg* "=" body
tag : ref (attr ("," attr)*)?
attr: id type
body:
| stmt
| exp
stmt: (indent exp)+
branch: (indent "|" unit " -> " exp)* # pattern match
#helper: : (indent attr)* (indent func)+
exp: formula branch?
formula:
| unit op2 formula # op2 => v + 1
| ref op2u formula # update => n += 1
| unit
unit:
| value
| id ("." id | "(" formula* ")")*
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
ie  : id formula
ee  : formula formula
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
'    # char?
@    # -
```

## 7. TODOs
- [] namespace and file structure
    public   : export(+)
    internal : ?
    private  : default
- [] compile to JavaScript
- [] self booting by compiled JavaScript
- [] make memo app
- [] dog fooding in real world
