# The Moa Programming Language
Moa is an open source programming language.
This language maximize productivity by fun programming.

Table of contents
1. Hello world
2. Syntax
3. Buildin
4. Appendix





## 1. Hello world

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
- test
main =
  v = 11
  test.eq(1 1)
  test.eq(12 v)
  test.eq(1 1)
```





## 2. Syntax
```
root: def (br def)*
def:
| "- " ref : ref+ # define namespace
| "- " ref        # use namespace
| ref+ ":" type+  # signature
| ref+ ":" iattr+ # struct
| ref+ ":" itag+  # enum
| func
func: id arg* "=" body
attr: id type
tag : ref (attr ("," attr)*)?
ifunc: indent func
iattr: indent attr
itag: indent tag
body:
| stmt
| exp helper?
stmt: (indent exp)+
helper: : iattr* ifunc+
exp: formula branch?
branch: (indent "|" unit " -> " exp)* # pattern match
formula:
| unit op2 formula # op2    : v + 1
| ref op2u formula # update : n += 1
| unit
unit:
| value
| id ("." id | "(" formula* ")")*
| "(" exp ")"
| "(" id+ => body ")" # lambda : (x y => x + y)
| "(" unit+ ")"       # tuple  : (1 n)
| "(" kv+ ")"         # struct : (name="value" age=30+7)

value:
| int    # 1
| float  # 1.0
| string # "hi"
| bool   # true
kv  : id ":" formula
id  : [a-z0-9_]+
ref : id (. id)*
type: ref ("(" ref+ ")")?
op2 : [; + - * / % & | << >> + - > >= < <=  == != || &&]
op2u: [= := += /= *= /= %=]
indent: "\n  "
br: "\n  "
```

## 3. Buildin

Reserved words
types
- bool, true, false
- int, float, string
- seq, array, set, dict, tuple, func
- opt, nil, do, try, error
- any, void
- i8, i16, i32, i64
- u8, u16, u32, u64
- f8, f16, f32, f64
- b8, b16, b32, b64
methods
- trace
values
- this
- io, eff

Binary operators order
```
* // / %                # number (high)
+ -                     # number (low)
> >= < <=  == !=        # comparision (high)
|| && | &               # comparision (low)
:= += /= *= /= %= .= <- # effect
,                       # no effect, just for readability
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
  t.done
  | _
  | t.cancel
  t.done.else(t.cancel)
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





## 4. Appendix

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
- Destructor
- Finalizer
- Globalization
- Type level programming
