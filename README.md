# The Moa Programming Language
Moa is an open source programming language.
This language maximize productivity by fun programming.

Table of contents
1. Hello world
2. Syntax
3. Buildin
4. Appendix





## 1. Getting started

Set up
```
mkdir -p ~/moa/bin
curl https://github.com/moa/releases/mac/latest > ~/moa/bin/moa
export PATH=$PATH:~/moa/bin
```

Make a project
```
# moa new
├README.md
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





## 2. Basic

Primitives
```
true # bool
1    # int 64bit signed integer
1.0  # real 64bit
"hi" # string utf8
```

Container
```
[1 2 3] # list
```

Expression
```
1 + (2 * 3)
(a b) => a + b
```

Types
```
Person:
  name string
  age int

Dict k v:
  values [](k,v)

Bool:
  True
  False

Maybe a:
  None
  Just a

Vector2:
  x,y int

Vector3:
  +Vector2
  z int

Int = I32
```

Function
```
pi = 3
inc x = x + 1
add x y = x + y
main = io:
  print (inc pi) (add 1 2)
```

Control Flow
```
gcd a b = if(
  a > b  -> gcd(b a)
  a == 0 -> b
  gcd((b / a) a))
```

Pattern Matching
```
show m = m
| None -> "none"
| Just(a) -> "just " ++ a.string
```

Effect
```
acc := 0
count <- calculate 1
acc += count
acc
```



## 3. Syntax
```
top: unit (op2 top)
unit: group | num | id | str
group: "(" top* ")" | "[" top* "]" | "{" top+ "}"
num: [0-9]+ (.[0-9]+)?
id: [A-Za-z_][A-Za-z0-9_]
str: '"' [^"] '"'
```

## 4. Buildin

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

Binary operators order
```
* // / %         # number (high)
+ -              # number (low)
> >= < <=  == != # comparision (high)
|| && | &        # comparision (low)
:= :             # define
+= -= *= /= %= = # effect
```

### IO

Standard input, output and error
```
main =
  line = io.readline
  line.empty.if(io.puts(line) io.warn(line))
```

File system
```
main =
  files = io.fs.ls("/tmp").filter(.is_file)
  contents = files.map(file => file.read)
```

TCP
```
main =
  handle packet =
    target = from.readline
    to = io.tcp.connect(target)
    from.bidirectional(to)
  io.tcp.listen("127.0.0.1:8080" handle)
```

UDP
```
main:
  handle packet =
    target = packet.string.lines.first
    io.udp.sendto(target packet)
  io.udp.bind("127.0.0.1:1234")
```

Async and Cancel
```
main =
  t = io.async(io.sleep(10))
  io.sleep(1)
  t.done.else(t.cancel)
```

Random
```
main =
  n = io.random.int(1 3)
  io.exit(n)
```

Time
```
main =
  now = io.now
  io.print(now)
```





## 5. Appendix

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
