# Feature
- pure function
- production type
- algebraic data type
- monadic statement
- type inference
- type class
- name space

# Rejection
- mutable
- null
- allocate / free
- macro
- class
- interface
- out of range access
- zero division
- any exception
- standard library
- GC

# Pending
- pointer
- weak reference
- using up many core
- asynchronous IO
- watch dog timer
- shared nothing for concurrency
- powerful logger, debugger, profiler and resources monitor
- strong composability
- type level programming

# Detail of name space
- x.y(z) is same as y(x z)
- x.y(z) allowed if x and y belong to same name space
- y(x z) allowed always
- core name space is always imported
  * statement: io, try, do, opt, asm
  * primitive: string, int, float, bool, true, false
  * structure: struct, enum
  * container: array, dict
```
# lib.moa
in lib
box:
  items array(int)
count: box int
count x = x.items.count

# app.moa
in app
use lib
size: box int
size x = x.count # ok, but can't call count(x)

is_empty: box bool
is_empty x = x.count == 0
status: box string
status x = is_empty(x) # ok, but can't call x.is_empty because box belongs to other name space
| true -> "empty"
| false -> "not empty"
```
