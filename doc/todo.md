# TODO (v0.1 self booting)
[x] Design error handling type and sequence syntax
[x] Tidy up documents
[x] Uniqueness
    1. pure
    2. readable short code
    3. it compiles to other languages
[] Design for exceptions
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
   # io = try + system call
   f : io(int)
   f = io.stdin.readline..to_int
   # panic = stsop coroutine due to critical error on runtime
   f : int
   f = int[int.max] # panic("out of memory") called by runtime
[] Design for effects
   - allow implicit effect
[] Minimal compiler to Ruby
[] Self booting with Ruby

# TODO (v0.2 Go and JavaScript)
[] Minimal compiler to Go
[] Making API server with Go
[] Minimal compiler to JavaScript
[] Making TODO app with Go and JavaScript

# TODO (v0.4 enhance built-in functions)

# TODO (v0.5 tracer)

# TODO (v1.0 relace)
