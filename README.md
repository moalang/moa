# The Moa Programming Language
Moa is an open source programming language that enhances your development experience.



## Why Moa?
By keeping your preferred development environment, you can enable highly powerful development support:
- Static type checking
- Powerful type inference
- Fast test execution
- Launch a debug shell on test failures



## Getting started

Install
```
bash -c "$(curl -fsS https://raw.githubusercontent.com/moalang/moa/main/bin/install.sh)" && exec $SHELL
```

Hello World
```
echo 'def main: log "Hello World"' > main.moa
moa run
```

```
Hello World
```



## Code generation for Go

handle.moa
```
struct Request:
  form dict string list[string]

struct Response:
  body bytes

export handle req.Request Response:
  {body=$"Hello {req.form:name}"}
```

handle.go
```
package main

# From handle.moa
type Request struct {
	form map[string][]string
}
type Response struct {
	body []byte
}
func handle(request Request) Response {
	body := []byte(fmt.Sprintf("Hello %s\n", request.form["name"]))
	return Response{body: body}
}
```

main.go
```
package main

import (
	"fmt"
	"log"
	"net/http"
)

func main() {
	log.Fatal(http.ListenAndServe(":3000", http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		req.ParseForm()
		resp := handle(Request{req.Form})
		w.Write(resp.body)
	})))
}
```

Start the server
```
moa go > handle.go
go run
```

Request to the server
```
curl -d 'name=Alice' http://localhost:3000/
```

Output
```
hello Alice
```



## LISP interpriter

main.moa
```
enum lisp:
  atom string
  pairs list lisp
  lambda:
    args list string
    body lisp
  
def main code:
  env = dict string lisp
  nodes = parse tokenize(code) 
  nodes.each node => log(show(run(env node)))

def tokenize code:
  r"([^ \r\n()]+|.)".split(code).filter(s => s.trim())

def parse tokens:
  var pos (-1)
  def consume:
    tokens[pos+=1]
  def items a:
    iif:
      pos >= tokens.size: throw "'(' is not closed"
      tokens[pos] == ")": pos += 1; a
      _: a ++ [unit()]
  def unit:
    s = consume()
    case s:
      "(": pairs items([])
      ")": throw "unexpected ')'"
      _: atom s
  items []

def show node:
  case node:
    atom a: a
    pairs []: "NIL"
    pairs l: (++ "(" l.map(show).join(" ") ")")
    lambda f: (++ "(lambda " f.args.map(show).join(" ") show(f.body) ")")

def run env node:
  t = atom "T"
  nil = pairs []
  case node:
    atom "t": t
    atom: node
    lambda: node
    pairs ["quote" x]                             : pairs x
    pairs ["atom" v.pairs([_ ...])]               : nil
    pairs ["atom" _]                              : t
    pairs ["eq" a b]                              : atom iif(show(a) == show(b) t f)
    pairs ["car" l.pairs([head ...])]             : head
    pairs ["cdr" l.pairs([_ ...tail])]            : pairs tail
    pairs ["cons" a b.pairs]                      : pairs [a] ++ b
    pairs ["if" a b c]                            : run env iif(run(env a) == nil c b)
    pairs ["lambda" args.pairs[atom] body]        : lambda args body
    pairs ["define" name.atom body]               : env.set name body
    pairs ["+" l.atom r.atom]                     : atom l.int + r.int
    pairs [f.lambda ...a] if f.args.size == a.size: run env ++ f.args.zip(a).dict f.body
    _                                             : throw $"{show node} is invalid"

test {eq}:
  def t expect code:
    eq expect show(run(dict() parse(code)))
  t "NIL" "(atom ())" 
  t "T" "(eq 1 1)" 
  t "1" "(car (quote (1 2 3)))"
  t "(2 3)" "(cdr (quote (1 2 3)))"
  t "(1 2 3)" "(cons 1 (quote (2 3)))"
  t "1" "(if t 1 2)"
  t "3" "((lambda (a b) (+ a b)) 1 2)"
  t "5" "(define a 2)(+ a 3)"
```

```
moa test
```

Output
```
........ok
```

```
moa run main "(+ 1 2)"
```

Output
```
3
```



## Moa command usage
```
Usage:
  moa                           # launch interactive shell
  moa env [+/-] [<version>]     # list versions; use, install or remove a version
  moa run [<func>] [<arg> ...]  # run Moa program
  moa test [<regexp> ...]       # test Moa program
  moa go [<package>]            # print Go
  moa js                        # print JavaScript
```


## Interactive shell commands 
```
:         -- repeat last command
:q        -- quit
:t <expr> -- show a type of <expr>
:p <expr> -- show consumed time for each functions of <expr>
:s <expr> -- show a summary of loops, branches, and thrown exceptions of <exprs>
```
