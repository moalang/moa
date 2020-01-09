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
value: 1, 1.0, "hi", [1 2], {a 1, b = a + 1}
  exp: f(x) + 1
const: n = 1
  def: now : time.now, inc n : n + 1, proc =(\n  exp)+
  var: n int, m int(1), o (1 + 2)
  set: n += 1, s := "hi"

### BNF
top: def ([\n=] def)*
def:
| func
| id id* ":" prop+ :: struct # v2: x int; y int
| id id* ":" tags+ :: enum   # b: t; f
body:
| exp (br if_bool){2} # switch to binary branch
| exp (br if_cond)+   # pattern match
| (br seq)+
seq:
| exp (; seq)*
| id "=" body # const
| id id+ "=" body # func
exp:
| tuple  :: unit ("," unit)+ # 1, 2
| op2    :: unit op2 exp     # v + 1
| set    :: ref op2u exp     # n += 1, s := "hi"
| lambda :: args => exp      # x, y => x + y
| unit
unit:
| value
| array  :: "[" unit* "]" # [1]
| call   :: id call*      # v, o.m(x y).v
| "(" exp ")"
| "(" id id* = exp ")"   # id x y = x + y
value:
| int    # 1
| float  # 1.0
| string # hi"
| bool   # true
  id: [a-z0-9_]+
 ref: id (. id)*
call: "." id | "(" exp* ")"
 op2: + - * / ...
op2u: += -= := ...
attr: id unit
func: id id* "=" body # now : time.now, add x y : x + y
prop: attr | func
 tag: id (prop (";" prop)*)?
tags: tag ([\n;] tag)*
args:
| "()"
| id (, id)*
if_bool: "|" exp
if_cond: "|" unit "=" exp

### Examples
ast:
  int n int
  op2 op string, left ast, right ast
parser:
  src string
  pos 0
  eq s =
    src.slice(pos).is_head(s) || error("miss")
    pos += s.length
    s
  read_int = many1(() => any("0123456789".to_a)).join("").to_i
  any xs = xs.one(y, ys => or(eq(y) any(ys)))
  or l r = bk = pos; l | pos := bk; r
  many1 f = [f()].concat(many(f))
  many f = (rec acc = rec(acc.push(f())) | acc)([])

main = parser("123").read_int
