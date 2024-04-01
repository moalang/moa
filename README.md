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
$ echo 'main: pr "Hello World"' > main.moa
$ moa run
Hello World
```

Compile
```
$ echo 'main: pr "Hello world"' > main.moa
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
Create a new project for web
```
$ moa new todo
$ tree .
┣ .gitignore
┣ main.moa
┗ public/
  ┣ apple-touch-icon.png
  ┣ favicon.svg
  ┣ favicon.ico
  ┗ og.png
```

You can see created files.
```# .gitignore
/a
/*.mdb
```

```# main.moa
let ttl_session 90days

record scheme:
  sessions dict string ref(user) ttl=ttl_session
  users    dict string user
  record user:
    id    int
    email string
    pwd   std.bcrypt
    todos list todo
  record todo:
    id    int
    title string
    memo  string

def main:
  let cookie_sid "sid"
  {request response} <- std.http.listen
  db <- std.db(scheme)
  let user db.sessions.get(request.cookie(cookie_sid)).default
  def html renderer:
    let html_headers [
      "cache-control"               , "private, max-age=0"
      "expires"                     , "-1"
      "permissions-policy"          , "unload=()"
      "content-security-policy"     , "default-src 'self'"
      "cross-origin-opener-policy"  , "same-origin"
      "cross-origin-resource-policy", "same-origin"
      "cross-origin-embedder-policy", "require-corp"
      "x-content-type-options"      , "nosniff"
    ]
    response.new(renderer.status html_headers renderer.string)
  def location loc:
    response.new(301 ["location",loc] "")
  def start_session user loc:
    sid std.rand.bytes(128).base64
    db.sessions.tie(sid user)
    location(loc).cookie(cookie_sid sid ttl=ttl_session) # by default, secure; httponly; samesite=lax; path=/
  def signup:
    {email password} = request.post
    guard !db.users.has(email) location("/signup?error=existed")
    let u scheme.user(email std.bcrypt("password")
    db.users.tie(email u)
    start_session(u "/")
  def signin:
    {email password} = request.post
    guard u:db.users.get(email).and(u => u.pwd.eq(password)) location("/signin?error=notfound")
    start_session(u "/")
  def new_todo:
    user.todos.push({...request.post})
    location("/")
  def update_todo:
    user.todos.update({...request.post})
    location("/")
  def signout:
    db.sessions.delete(request.cookie(cookie_sid))
    location("/").cookie(cookie_sid "" ttl=0)
  let mp request.method ++ " " ++ request.path
  if:
    mp == "post /signup"         : signup()
    mp == "post /signin"         : signin()
    mp == "post /new/todo"       : new_todo()
    mp == "post /up/todo"        : update_todo()
    mp == "get /signout"         : signout()
    std.fs("public").exists(path): response.file(path headers=["cache-control","public, max-age=3600"])
    template.match(path)         : html(template.dispatch({request user}))
    html(template.render.notfound({request}))

let template std.html5::
  "/":
    layout `Todos ($user.todos.size)`:
      a@new href=/todo | New Todo
      if user:
        h2 "Todos"
        ul
          for user.todos todo =>
            li a@item href=/todos/$todo.id $todo.title
      else:
        a@signin href=/signin | Signin
        a@signup href=/signup | Signup
  
  "/todos":
    layout "New Todo":
      form@new action=/new/todo method=post:
        | Title
        input type=text name=title
        | Memo
        textarea name=memo
        button | Submit
  
  "/todos/:id.int":
    guard todo = user.todos.get(id) notfound
    layout todo.title:
      h1 | $todo.title
      pre $todo.memo
      form@update action=/up/todo method=post:
        input type=hidden name=id value=$todo.id
        | Title
        input type=text name=title value=$todo.title
        | Memo
        textarea name=memo $todo.memo
        button | Update
  
  "/signin":
    layout "Signin":
      if get("error"):
        p | Failed to signin
      form@signin action=/signin method=post:
        | Email
        input type=text name=email
        | Password
        input type=text name=password
        button | Signin
  
  "/signup":
    layout "Signup":
      if get("error"):
        p | Email already registered
      form@signup action=/signup method=post:
        | Email
        input type=text name=email
        | Password
        input type=text name=password
        button | Signup
  
  notfound:
    layout "Not found":
      status = 404
      | Not found content.
      a href=/ | Back to top
  
  layout title body:
    var description "Example of Moa programming language"
    !doctype html
    html lang=ja:
      head:
        meta charset=utf-8
        title $title
        meta name=viewport content=width=device-width,initial-scale=1,shrink-to-fit=no,viewport-fit=cover
        link rel=icon type=image/svg+xml href=/favicon.svg
        link rel=apple-touch-icon type=image/png href=apple-touch-icon.png sizes=180x180
        meta name=theme-color content=#ffffff
        meta name=description content=$description
        meta property=og:image content=$request.origin/og.png
        meta property=og:locale content=ja_JP
        meta property=og:site_name content="Moa Todo App"
        meta property=og:title content=$title
        meta property=og:description content=$description
        meta property=og:url content=$request.url
        meta property=og:type content=website
        meta name=twitter:card content=summary_large_image
      body:
        header:
          h1 a href=/ | Todo App
          a@signout href=/signout | Signout
        $body
        footer | &copy; example.com

test t:
  db <- t.db(scheme)
  b <- t.browse("/")
  b.a.signup
  b.form.signup({email="foo@example.com" memo="bar"})
  b.a.new
  b.form.new({title="hello" memo="world"})
  b.a.item
  b.form.update({title="Hello"})
  t.browse(b.url r => t.eq 404 r.status)

  t.eq(1 db.users.size)
  let user db.users.0
  t.eq("foo@example.com" user.email)
  t.eq(true user.pwd.eq("bar"))
  t.eq([{id=0 title="Hello" memo="world"}] user.todos)
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

See mdb file
```
$ moa cat test.mdb
# 2024/01/01 00:00:00
users.push {id=0 email="foo@example.com" pwd="..."}
sessions.set "..." ref(0)
# 2024/01/01 00:00:00
users.0.todos.push {id=0 title="hello" memo="world"}
# 2024/01/01 00:00:00
users.0.todos.0.title = "Hello"
```



## Manual for moa command
```
Usage:
  moa <command> [arguments]

The commands are:
  moa                   # launch REPL
  moa build [os] [arch] # compile to executable file
  moa new [template]    # create a new project
  moa run               # run the program
  moa test [path] ...   # run tests
  moa cat [path] ...    # show mdb file as text
```
