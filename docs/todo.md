# ToDo
- [ ] Create bootstrap.js to execute moa.moa
- [ ] Generate and run Go code from Moa source code
- [ ] Implement standard libraries

bootstrap.js
- [x] Enable io.put and io.puts
- [x] Enable primitives
- [x] Enable throw and catch
- [x] Enable list
- [x] Enable map
- [x] Enable def
- [x] Enable class
- [x] Enable var
- [x] Enable let
- [x] Enable test
- [x] Enable if
- [x] Enable else
- [x] Enable iif
- [x] Enable for
- [x] Enable each
- [ ] Enable while
- [ ] Enable continue
- [ ] Enable break
- [ ] Enable return

moa.moa
- [ ] Enable tokenizer
- [ ] Enable parser
- [ ] Enable type inference
- [ ] Enable Go code generator
- [ ] Enable REPL

std.go
- [ ] io.put
- [ ] io.puts
- [ ] io.log
- [ ] io.now
- [ ] io.rand
- [ ] io.http
- [ ] io.db
- [ ] io.fs



## Syntax
```
top: line ("\n" line)*
line: exp+ comment?
exp: unit (op2 exp)?
unit: atom no-space suffix*
suffix:
| "." id
| "(" exp* ")"
| "[" exp* "]"
| "{" exp* "}"
atom:
| "(" exp ")"
| "[" exp* "]"
| "{" top "}"
| [0-9]+ ("." [0-9]+)?
| '"' .* '"'
| id
op2: [+-*/%<>|&^=!]+
id: [A-Za-z_][A-Za-z0-9_]*
comment: "//" .*
reserved: def class enum var let test if else iif switch match for each while continue break return yield package import export
```

## Keyword
Primitive
- bool, true, false
- int
- float
- string
- fn
- i8, i16, i32, i64, u8, u16, u32, u64, f8, f16, f32, f64

Container
- opt, some, none
- wref
- tuple
- struct
- list
- map
- set
- time
- task
- stream, reader, writer

Modifier
- eq
- ord
- hash
- show
- num
- iter

Declaration
- def
- class
- enum
- var
- let
- test

Branch
- if
- else
- iif
- switch
- match

Control flow
- for
- each
- while
- continue
- break
- yield
- return
- throw, catch

Namespcae
- packgage
- import
- export

IO
- io.put
- io.puts
- io.log
- io.now
- io.rand
- io.http
- io.db
- io.fs

Binary operators
```
:                 # tag
?? ?.             # optional
* / %             # number (high)
+ -               # number (low)
| & ^ << >>       # integer
|| &&             # boolean
> >= < <=  == !=  # comparision
= += -= ...       # updation
```
