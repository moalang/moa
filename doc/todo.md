# TODO (v0.1 self booting)
[x] Design error handling type and sequence syntax
[x] Tidy up documents
[x] Uniqueness
    1. compile to other languages
    2. no system call
    3. monad like error handling
    ```
    find a : a.array a try(a)
    main =
      hit <- find([1 2 3] 1)
    ```
    4. monad like variables
    ```
    n var(int)
    incr : eff(int)
    incr = n += 1
    ```
    5. minimal syntax
    ```
    exp
    | error case
    | success case
    exp
    | matcher : exp
    | _ : exp
    ```
[] Design for exceptions
   - should be explicit throwing in type system
   - how to throw and switch specific error type?
   - allow implicit throwing in code level?
[] Design for effects
   - allow implicit effect?
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
