# TODO (v0.1 self booting)
[x] Design error handling type and sequence syntax
[x] Tidy up documents
[x] Uniqueness
    1. pure
    2. readable short code
    3. it compiles to other languages
[x] Error handling design
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
   # io = system call
   f : io(int)
   f = io.stdin.readline..to_int
   # panic = unrecoverable errors
   f : array(int)
   f = int[int.max] # panic("out of memory")
   f = panic("unreachable")
[x] Minimal compiler to Ruby
[x] Self booting with Ruby

# TODO (v0.2 JavaScript)
[x] Minimal compiler to JavaScript
[] Feedback to syntax from making minimal compiler to JavaScript
[] Making API server by nodejs
[] Making Kakeibo app on GAE

# TODO (v0.4 enhance built-in functions)

# TODO (v0.5 tracer)

# TODO (v1.0 relace)
