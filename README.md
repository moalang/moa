# The Moa Programming Language
Moa is an open source programming language that enhances your development experience.



## Why Moa?
For powerful type inference, smooth scripting, high speed, and great debugging, Moa is the way to go.



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



## HTTP server with transactional database

main.moa
```
struct schema:
  pv int

def main:
  std.http.listen {port=3000} req =>
    std.db[schema] db =>
      db.pv += 1
      {body=$"hello {req.name?}, pv is {db.pv}"}
```

Start the server
```
moa run
```

Request to the server
```
curl -d 'name=Alice' http://localhost:3000/
```

Output
```
hello Alice, pv is 1
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
  
def main:
  var env dict string lisp
  parse(tokenize(io.stdin.utf8)).each(node => show(run(env node)))

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
echo "(+ 1 2)" | moa run
```

Output
```
3
```



## Moa command usage
```
Usage:
  moa                           # launch interactive shell
  moa build [<os>] [<arch>]     # compile Moa program
  moa dev [<port>] [<ssh://..>] # launch developer console as http sever
  moa deploy [<ssh://..> ...]   # compile, deploy, and run on remote hosts
  moa env [+/-] [<version>]     # list versions; use, install or remove a version
  moa help                      # show usage of moa command
  moa run [<exp>]               # run Moa program
  moa test [<regexp> ...]       # test Moa program
```


## Interactive shell commands 
```
:         -- repeat last command
:q        -- quit
:t <expr> -- show a type of <expr>
:p <expr> -- show consumed time for each functions of <expr>
:s <expr> -- show a summary of loops, branches, and thrown exceptions of <exprs>
```
