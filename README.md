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
Create from template
```
$ moa new web
└ src
   ├ main.moa
   └ public
      ├ common.css
      ├ common.js
      ├ logo.png
      ├ og.png
      ├ apple-touch-icon.png
      └ favicon.ico
```

See programs
```
$ cat src/main.moa
record todo:
  title string
  memo  string

record scheme:
  todos list todo

def main:
  {method path post location html mount} <- io.http.listen
  {todos} <- io.db scheme
  if:
    method == "post" && path == "/api/todos":
      todos.push post
      location "/" 
    method == "post" && path.starts("/api/todos/"):
      todos.tie post("id").int post
      location path
    method == "get" && path == "/":
      html index
    method == "get" && r"/todos/(?<id.int>\d+)".match(path) if todo=todos[id]:
      html todo({todo})
    mount "./public"
  
let index layout "title" "desc":
  h1 | Example
  - for todos todo =>
    h2 a href=/todos/$todo.id $todo.title
    form action=/todos method=post
      | Title
      input type=text name=title
      | Memo
      textarea name=memo
      button | Submit

let todo layout "title" "desc":
  h1 | $todo.title
  $todo.memo
  form action=/todos/$todo.id method=post
    | Title
    input type=text name=title value=$todo.title
    | Memo
    textarea name=memo $todo.memo
    button | Update

def layout title desc @body:
  library.template {title desc}:
    doctype html
    html lang=ja
      head
        meta charset=utf-8
        title $title
        meta name=viewport content=initial-scale=1,minimum-scale=1,width=device-width
        meta name=description content=$desc
        meta name=theme-color content=#0878C9
        link rel=manifest href=/manifest.webmanifest
        link rel=icon href=/favicon.ico
        link rel=apple-touch-icon href=/apple-touch-icon.png
        link rel=stylesheet href=/common.css
        meta property=og:title content=$title
        meta property=og:description content=$desc
        meta property=og:type content=$(if req.path=="/" "website" "article")
        meta property=og:url content=$req.url
        meta property=og:image content=/og.png
        meta property=og:site_name content="Todo site"
        meta property=og:locale content=ja_JP
        meta name=twitter:card content=summary
      body
        header
          h1 a href=/ img src=/logo.png | Example Application
        $body
        footer
          | &copy; $req.domain
        script src=/common.js

test t:
  let {get post location contains} t.http
  get "/":
    status 200
  get "/todos/1":
    status 404
  post "/todos" title="hello" memo="world":
    status 301
    location "/"
  get "/todos/1":
    status 200
    contains "hello" "world"
  post "/todos" id=1 title="hello" memo="world":
    status 301
    location "/"
  get "/todos/1":
    status 200
    contains "hello!" "world!"
```

Access
```
$ moa run
http://localhost:8000
```

Run test
```
$ moa test
```

Compile and deploy
```
$ moa deploy linux x86 8000 ssh://user@hostname:~/
```



## Manual for moa command
```
Usage:
  moa <command> [arguments]

The commands are:
  moa                                     # launch REPL
  moa build [os] [arch]                   # compile to executable file
  moa deploy [os] [arch] [port] ssh://... # compile and deploy to server
  moa new [platform]                      # make a project
  moa run                                 # run the program
  moa test [path] ...                     # run tests
```
