# The Moa Programming Language
Moa is an open source programming language that enhances your development experience.



## Getting started

Install
```
bash -c "$(curl -fsS https://raw.githubusercontent.com/moalang/moa/main/misc/install.sh)" && exec $SHELL
```

Hello world
```
echo 'def main io.puts("Hello world")' > hello.moa
moa dev hello.moa
```

```
Hello world
```


## REPL
```
moa repl
```

```
> 1 + 2
3
```


## Example: HTTP server

http.moa
```
def main {
  io.http.serve ":8000" req => (text:"hello")
}

def test t {
  t.eq "hello" main.get("/").body
}
```

```
moa test http.moa
```

```
.ok
```

```
moa dev http.moa
```

```
curl http://localhost:8000
```

```
hello
```



## Example: ToDo application

http.moa
```
struct DB {
  todos list[Todo]
}

struct Todo {
  id         i64
  title      string
  done       bool
  created_at time
}

def main {
  io.http.serve ":8000" req => io.db[DB] "db.jsonl" db => match {
    (path:"/")                    io.fs("index.html")
    (path:"/todos" method:"get")  (json:db)
    (path:"/todos" method:"post") {
      db.todos.push title:req.post("title") created_at:io.now
      (status:204)
    }
  }
}
```

```
moa dev http.moa
```

```
curl -d "title=walking" http://localhost:8000/todos
curl http://localhost:8000/todos
```

```
{"todos": [{"title":"walking","created_at":"2000-01-02T03:04:05Z"}]}
```

```
cat db.jsonl
```

```
{"todos": [{"title":"walking","created_at":"2000-01-02T03:04:05Z"}]}
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
reserved: def class enum var let test if else iif switch match for while continue break return yield package import export
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

Modifier
- eq
- ord
- hash
- show
- num
- enum

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
- io.fs
- io.http
- io.db

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
