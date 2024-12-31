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
