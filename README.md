# The Moa Programming Language
Moa is an open source programming language that enhances your development experience.



## Why Moa?
For powerful type inference, smooth scripting, high speed, and great debugging, Moa is the way to go.



## Getting started

Install
```
bash -c "$(curl -fsS https://github.com/moalang/moa/install.sh)"
exec $SHELL
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
  io.http.listen req =>
    io.db[schema] db =>
      db.pv += 1
      {body=$"hello {req.name?}, pv is {db.pv}"}
```

```
moa watch
curl -d 'name=Alice' http://localhost:3000/
```

Output
```
hello Alice, pv is 1
```

You can also access developer console via `http://localhost:3001`.



## LISP interpriter

main.moa
```
enum lisp:
  atom string
  list array lisp
  lambda:
    args array string
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
  def slist a:
    iif:
      pos >= tokens.size: throw "'(' is not closed"
      tokens[pos] == ")": pos += 1; a
      _: a ++ [unit()]
  def unit:
    s = consume()
    case s:
      "(": list slist([])
      ")": throw "unexpected ')'"
      _: atom s
  slist []

def show node:
  case node:
    atom a: a
    list []: "NIL"
    list l: (++ "(" l.map(show).join(" ") ")")
    lambda f: (++ "(lambda " f.args.map(show).join(" ") show(f.body) ")")

def run env node:
  t = atom "T"
  nil = list []
  case node:
    atom "t": t
    atom: node
    lambda: node
    list ["quote" x]                             : list x
    list ["atom" v.list([_ ...])]                : nil
    list ["atom" _]                              : t
    list ["eq" a b]                              : atom iif(show(a) == show(b) t f)
    list ["car" l.list([head ...])]              : head
    list ["cdr" l.list([_ ...tail])]             : list tail
    list ["cons" a b.list]                       : list [a] ++ b
    list ["if" a b c]                            : run env iif(run(env a) == nil c b)
    list ["lambda" args.list[atom] body]         : lambda args body
    list ["define" name.atom body]               : env.set name body
    list ["+" l.atom r.atom]                     : atom l.int + r.int
    list [f.lambda ...a] if f.args.size == a.size: run env ++ f.args.zip(a).dict f.body
    _                                            : throw $"{show node} is invalid"


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
  moa                   # launch interactive shell
  moa build [os] [arch] # compile
  moa run [exp]         # run
  moa test [regexp] ... # test
  moa watch             # run, re-run on changes
```
