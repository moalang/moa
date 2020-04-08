# The list of common tendency and measures
The followings are well known problems and measures of moa language.



## About usability

### Boilerplate codes
Provide shorthand way.
- for parallel: pmap, peach, pfilter
- for error handling: ignore, alternative, switching

### Resource leaks
Force RAII pattern.
Provide weak reference for caching.

### Inheritance complexity
No inheritance.
Provide interface which can have implementation.



## About reliability

### Null pointer reference
Type system don't permit implicit nullable.

### Out of range
Type system provide contract feature?

### Zero division
Type system don't permit to ignore zero division.

### Exception problem
No exception.

### Ignore error
Type system automatically propagate errors unless there is explicit handling.

### Unavoidable errors like out of memory, stack overflow and unexpected infinity loop
Provide a feacher which is constrained memory usage, execution time and some system call.



## About performance

### C10K problem
Provide coroutine and threads like Go language.

### GC overhead
No GC.

### VM overhead
No VM. Using native code from LLVM.



## About concurrency
Shared nothing.
Named database provide transaction, ownership, references and borrowing for share.



## About diagnosability
Provide logger, debugger, profiler, resources monitor and memory dump.



## About composability

### Error composability
All errors could be composable.

### Different language version composability
Latest compiler can compile and link all old version codes.

### Different library version composability
Each versioned library is separated.
Between two, object can be passed if same memory structed.



## Syntax
```
value: 1, 1.0, "hi"
  box: array(1 2), tuple(1 2), set(1 2), struct(a:1 b:2), dict("a"+1 "b"+2)
  exp: f(x) + 1
  eff: n += 1
const: n = 1
 func: inc x = x + 1
 stmt: echo = line <- io.stdin.readline; io.stdout(line)
  var: n int

add x y = add(x y 0)
add x y z = x + y + z

method override?
  optional artuments:
    add x y z:0 = x + y + z
    add x y z: = x + y + z.or(0)
    add(1 2)
    add(1 2 3)
  variable artuments:
    trace label args* = print(label args.join(" "))
    trace("title" "a" "b" "c")

instead of structure arguments?
  keyward artuments:
    add x:0 y:0 = x + y
    add(1 2)
    add(1 y:2)
    add(x:1 y:2)
    add(y:1 x:2)
instead of just pass to dictionary?
  dictionary artuments:
    calc d** = d("a" 0) + d("b" 0)


add x y:0 z = x + y + z("n" 0) + z("m" 0)
 call: add(1 y:2 "n"?3 "m"?4)
```

### BNF

### Examples
enum ast:
  int int
  op2:
    op string
    left ast
    right ast

parser src = read_int:
  pos int
  read_int = many1(satisfy(1 "0123456789".has))).join("").to_i
  satisfy n f =
    (pos >= src.count) || err("eof")
    s <- src.slice(pos n).assert("bug")
    f(s) || err("not satisfy" n f)
    pos += s.length
    s
  many1 f =
    x <- f
    xs <- many(f)
    [x] ++ xs
  many f = go:
    acc array
    go = f.fmap(acc.push); go

main = parser("123").read_int
