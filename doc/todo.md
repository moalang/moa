# TODO (v0.1 self booting)
[x] Design error handling type and sequence syntax
[x] Tidy up documents
[] Uniqueness
    1. compile to other languages
    2. monad like error handling
    ```
    find a : a.array a try(a)
    main =
      hit <- find([1 2 3] 1)
    ```
    3. monad like variables
    ```
    n var(int)
    incr : eff(int)
    incr = n += 1
    ```
    4. branch syntax
    ```
    exp.if(true false)
    exp
    | error case
    | success case
    exp
    | matcher = exp
    | _ = exp
    ```
[] Minimal to Go compiler
    ```
    main : io(int)
    main _ = 0
    ```
[] Support IO
    ```
    main : io(int)
    main io =
      now <- io.now
      io.stdout(now.format("YYYY/MM/DD hh:mm:ss"))
      0
    ```
[] Support primitive values
    - bool
    - int
    - float
    - string
    - func
    - array
    - map
    - class
    - enum
[] Support define
[] Support branch
[] Support variable
    - var
    - eff
[] Self booting
# TODO (v0.2 core library)
# TODO (v0.3 standard library)
