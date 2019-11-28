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
Type system provide contract feature.

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
All errors can be composable.

### Different language version composability
Latest compiler can compile and link all old version codes.

### Different library version composability
Each versioned library is separated.
Between two, object can be passed if same memory structed.
