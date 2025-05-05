# The Moa Programming Language
Moa is an open source programming language that enhances your development experience.



## Getting started

Install
```
bash -c "$(curl -fsS https://raw.githubusercontent.com/moalang/moa/main/misc/install.sh)" && exec $SHELL
```

Hello world
```
echo 'main io.puts("Hello world")' > hello.moa
moa run hello.moa
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
main = io.http.serve ":8000" req => text:"hello"

test t:
  t.get("/").body.eq("hello")
```

```
moa test http.moa
```

```
1 / 1 100%
```

```
moa run http.moa
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
Schema =
  todos vec[Todo]

Todo =
  id         i64
  title      string
  done       bool
  created_at time

main =
  db <- io.db[Schema] "db.jsonl"
  req <- io.serve ":8000"
  dispatch db req
  | e => (status=500 text="internal server error")

dispatch db req = req.route
| "get /"       file:"index.html"
| "get /todos"  json:db
| "post /todos"
   db.todos.push req{title} created_at:io.now()
   status:204
| "get /todos/:id`
   db.todos.merge req{id title done}
   status:204
| "delete `/todos/:id`
   db.todos.delete req{id}
   status:204
| _ status:400 text:"not found"
```

```
moa run http.moa
```

```
curl -d "title=walking" http://localhost:8000/todos
curl http://localhost:8000/todos
```

```
{"todos": [{"id":1,"title":"walking","created_at":"2000-01-02T03:04:05Z"}]}
```

```
cat db.jsonl
```

```
{"todos": [{"id":1,"title":"walking","created_at":"2000-01-02T03:04:05Z"}]}
```
