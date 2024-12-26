# The Moa Programming Language
Moa is an open source programming language that enhances your development experience.



## Getting started

Install
```
bash -c "$(curl -fsS https://raw.githubusercontent.com/moalang/moa/main/bin/install.sh)" && exec $SHELL
```

Hello world
```
echo 'def main puts("Hello world")' | moa
```

```
Hello world
```



## Example: HTTP server

http.moa
```
use http

def main {
  http.serve "localhost:8000" req => (type:"text/plain" body:"hello")
}

def test t {
  t.eq "hello" main.get("/").body
}
```

```
moa test
```

```
.ok
```

```
moa run
```

```
curl http://localhost:8000
```

```
hello
```



## Moa command usage
```
Usage:
    moa                   # launch interactive shell
    moa run               # run the program
    moa test              # test the program
    moa build [os] [arch] # compile to executable file
    moa watch [...]       # Run command and again when a file is changed
```



## Interactive shell commands 
```
:          -- repeat last command
:q         -- quit the shell
```



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
reserved: def class trait enum var let test if else when iif switch match for while continue break return yield package import export
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
- ref, wref
- tuple
- struct
- list
- map
- set
- time
- async
- stream, reader, writer

IO
- now
- random
- tcp
- fs

Declaration
- def
- class
- trait
- enum
- var
- let
- test

Branch
- if
- else
- when
- iif
- switch
- match

Control flow
- for
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

Binary operators
```
??                # opt
* / %             # number (high)
+ -               # number (low)
| & ^ << >>       # integer
|| &&             # boolean
> >= < <=  == !=  # comparision
= += -= ...       # updation
```
