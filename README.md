# The Moa Programming Language
Moa is an open source programming language.
This language maximize productivity by fun programming.



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
.x Failed
test.moa:5|  eq(1 2)
expect: 1
  fact: 2

# test.moa
- test
main =
  test.eq(1 1)
  test.eq(1 2)
```





## 2. Basic

Primitives
```
true # bool
1    # int 64bit signed integer
1.0  # float 64bit
"hi" # string utf8
```

Container
```
(1 2)             # tuple
(a=1 b=2)         # named tuple
[1 2 3]           # list []
["one":1 "two":2] # hash [:]
```

Anonymouse Function
```
a => a
(a b) => a + b
```

Types
```
person:: name age
dict k v:: values[(k v)]
bool:| true false
maybe a:| none just(a)
```

Function
```
pi: 3
inc x: x + 1
add x y: x + y
```

Control Flow
```
gcd a b: if(
  a < b  gcd(b a)
  b == 0 a
  _      gcd(b a/b))

show m: match(m
  none "none"
  just (++ "just " m))
```

Effect
```
main:
  line := io.readline # := allow override
  line = "> " + line
  io.print(line)
```



## 3. Syntax
```
top: func | data | adt | scope
func: id+ "=" exp
data: id+ ":" (br attr)+
adt: id+ "|" (br id attr*)+
scope:
| "-" id+ (":" (br id type*)+)?

body: (br exp)+ | exp
exp: unit (op2 exp)?
unit:
| "(" exp+ ")" call?
| "[" exp* "]"
| [0-9]+ (.[0-9]+)?
| '"' [^"] '"'
| id call?
call: "(" exp+ ")"
op2: + - * / // % = += -= *= /= == != || && >= > <= < =>

id: [A-Za-z_][A-Za-z0-9_]
attr: id type
type: id | "[" type (":" type)? "]" | "(" type type+ ")"
```

## 4. Buildin

- bool: true false
- int, float, string, i8..i64, u8..u64, f32,f64
- error: nil

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
  line <- io.reads
  when(line.empty ->
    io.puts(line)
    io.warn(line))
```

File system
```
main =
  files <- io.fs.ls("/tmp").filter(.is_file)
  files.each(f => io.write(f.reads))
```

TCP
```
main =
  handle socket =
    socket.lines.each(line =>
      socket.write(line))
  io.tcp.listen("127.0.0.1:8080" handle)
```

UDP
```
main:
  handle socket =
    target <- socket.line
    io.udp.copy(target socket.read)
  io.udp.bind("127.0.0.1:1234")
```

Async and Cancel
```
main =
  t <- io.async(io.sleep(10))
  io.sleep(1)
  when(t.done t.cancel)
```

Random
```
main =
  n <- io.random(1 3)
  io.exit(n)
```

Time
```
main =
  now <- io.now
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
