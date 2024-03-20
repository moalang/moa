# The Moa Programming Language
Moa is an open source programming language which helps programming for fun!



## Getting started

Install
```
bash -c "$(curl -fsS https://github.com/moalang/moa/install.sh)"
exec $SHELL
```

Hello World
```
$ echo 'def main: io.puts "Hello World"' > main.moa
$ moa run
Hello World
```

Compile
```
$ echo 'def main: io.puts "Hello world"' > main.moa
$ moa build
$ ./a
Hello world
```

Launch REPL
```
$ moa
> 1 + 2
3
> main()
Hello world
```

## Example: ToDo app
Create a file `main.moa`.
```
record todo:
  title string
  memo  string

record scheme:
  todos list todo

def main:
  {method path post status location html} <- io.http.listen
  {todos} <- io.db scheme
  if:
    method == "post" && path == "/api/todos":
      todos.push post
      location "/" 
    method == "post" && path.starts("/api/todos/"):
      todos.tie post("id").int post
      location path
    method == "get" && path == "/":
      html template.index({todos})
    method == "get" && r"/todos/(?<id.int>\d+)".match(path) && todo <- todos[id]:
      html template.todo({todo})
    status 404

let template lib.html5:
  @index todos
  layout `Todos ($todos.size)`:
    - for todos todo =>
      h2 a href=/todos/$todo.id $todo.title
    form action=/todos method=post
      | Title
      input type=text name=title
      | Memo
      textarea name=memo
      button | Submit
  
  @todo todo
  layout todo.title:
    h1 | $todo.title
    pre $todo.memo
    form action=/todos/$todo.id method=post
      | Title
      input type=text name=title value=$todo.title
      | Memo
      textarea name=memo $todo.memo
      button | Update
  
  @layout title body
  !doctype html
  html lang=ja
    head
      meta charset=utf-8
      title $title
    body
      header h1 a href=/ | Todo App
      $$body
      footer | &copy; example.com

test t:
  let {get post} t.http
  def has path ...s:
    get path r =>
      r.status 200
      r.contains ...s
  def up path location param:
    post path param:
      r.status 301
      r.header "location" location
  has "/" "example.com"
  get "/todos/0" r => r.status 404
  up "/todos" "/" {title="hello" memo="world"}
  has "/todos/0" "hello" "world"
  up "/todos" "/" {id=1 title="hello" memo="world"}
  has "/todos/0" "Hello" "world"
  t.eq [todo("Hello" "world.")] t.io.db(scheme).todos
```

Execute the program, then you can access `http://localhost:8000`.
```
$ moa run
http://localhost:8000
```

Run test
```
$ moa test
```

Minimize mdb file
```
$ moa cat test.mdb
# 2024/01/01 00:00:01
todos.push({title="hello" memo="world"})
# 2024/01/01 00:00:02
todos[0].title="Hello"

$ moa min test.mdb

$ moa cat test.mdb
# 2024/01/02 00:00:03
todos.append([{title="Hello" memo="world"}])
```



## Manual for moa command
```
Usage:
  moa <command> [arguments]

The commands are:
  moa                   # launch REPL
  moa build [os] [arch] # compile to executable file
  moa run               # run the program
  moa test [path] ...   # run tests
  moa cat [path] ...    # show mdb file as text
  moa min [path] ...    # minimize size of mdb file
```
