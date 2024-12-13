# The Moa Programming Language
Moa is an open source programming language that enhances your development experience.



## Getting started

Install
```
bash -c "$(curl -fsS https://raw.githubusercontent.com/moalang/moa/main/bin/install.sh)" && exec $SHELL
```

Hello world
```
echo 'main: puts "Hello world"' | moa
```

```
Hello world
```



## Example: HTTP server

http.moa
```
use http

main: http.serve "localhost:8000" req => (type:"text/plain" body:"hello")

test {eq}: eq "hello" main.get("/").body
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
    moa run  [--watch]    # run the program
    moa test [--watch]    # test the program
    moa build [os] [arch] # compile to executable file
```



## Interactive shell commands 
```
:          -- repeat last command
:q         -- quit the shell
```



## Primitive
- bool
- int
- float
- string
- function
- tuple
- struct

## Standard library
- optional
- list
- map
- set
- time
- random
- stream
- error



## Syntax
```
top: (line (br line)*)?
line: (reserved exp*) | exp
exp: unit (op2 exp)?
unit: atom no-space suffix*
suffix:
| "." id
| "(" exp* ")"
| "[" exp* "]"
atom:
| "(" exp ")"
| "{" top "}"
| [0-9]+ ("." [0-9]+)?
| '"' .* '"'
| id
op2: [+-*/%<>|&^=!]+
id: [A-Za-z_][A-Za-z0-9_]*
br: "\n" | ";"
reserved: def struct enum var let if else match for while test continue break return package import export
```

## Examples
```
def fib n:
  var a 0 b 1
  while a < n:
    yield a
    a, b = b, a + b

def fib n var(a 0 b 1
  while a < n {
    yield a
    a, b = b, a + b
  })

def gcd x y with(
  gcd_ a 0 a
  gcd_ a b gcd_(b a.rem(b))
  gcd_(x.abs b.abs))

gcd x y =  gcd' (abs x) (abs y)
           where gcd' a 0  =  a
                 gcd' a b  =  gcd' b (a `rem` b)
```
