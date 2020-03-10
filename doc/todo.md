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
    exp.if(true false)
    exp
    | error case
    | success case
    exp
    | matcher = exp
    | _ = exp
    ```
[] Minimal compiler to make JavaScript
    ```
    main = 0
    --
    function moa_main() { return 0 }
    ```
[] Minimal compiler to make Go
    ```
    main = 0
    --
    func moa_main() int64 { return 0 }
    ```
[] Support primitive values
    - bool
    - int
    - float
    - string
    - closure
[] Support function
    - func
[] Support containers
    - array
    - map
    - struct
    - enum
[] Support variable
    - var
[] Support io
    - in
    - out
    - io
[] Support expression
[] Support branch
[] Support standard library for values
[] Self booting
# TODO (v0.2 core library)
# TODO (v0.3 standard library)
