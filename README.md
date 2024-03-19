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
Create files below.
```
# main.moa
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
      html index({todos})
    method == "get" && r"/todos/(?<id.int>\d+)".match(path) && todo <- todos[id]:
      html todo({todo})
    status 404

def index:
  layout title="List of TODOs":
    h1 | Example
    - for todos todo =>
      h2 a href=/todos/$todo.id $todo.title
      form action=/todos method=post
        | Title
        input type=text name=title
        | Memo
        textarea name=memo
        button | Submit

def todo:
  std.template:
    layout title=$todo.title:
      h1 | $todo.title
      $todo.memo
      form action=/todos/$todo.id method=post
        | Title
        input type=text name=title value=$todo.title
        | Memo
        textarea name=memo $todo.memo
        button | Update

def layout:
  std.template:
    doctype html
    html lang=ja
      head
        meta charset=utf-8
        title $title
      body
        header
          h1 a href=/ | Example Application
        $(std.template body)
        footer
          | &copy; example.com

test t:
  let {get post header contains} t.http
  def ok s:
    status 200
    contains s
  def location value:
    status 301
    header "location" value

  get "/":
    ok "example.com"
  get "/todos/1":
    status 404
  post "/todos" title="hello" memo="world":
    location "/"
  get "/todos/1":
    ok "hello" "world"
  post "/todos" id=1 title="hello" memo="world":
    location "/"
  get "/todos/1":
    ok "hello!" "world!"
```

Execute the program, then you can access `http://localhost:8000` by your browser.
```
$ moa run
http://localhost:8000
```

Run test
```
$ moa test
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
```
