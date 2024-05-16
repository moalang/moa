# The Moa Programming Language
Moa is an open source programming language that enhances your development experience, especially for web development.



## Why Moa?
If some of the following are bothering you, Moa can help.

- Struggle with debugging  
  → Type error reporting during compilation  
  → Highly flexible debugger
- Slow compilation speed  
  → Fast compiler
- Complex environment setup  
  → Integrated setup for development, staging, and production environments  
  → Hot deployment  
  → Embedded dashboard for developer



## Getting started

### Install
```
bash -c "$(curl -fsS https://github.com/moalang/moa/install.sh)"
exec $SHELL
```

### Hello World
```
echo 'main: pr "Hello World"' > main.moa
moa run
```

Output
```
Hello World
```

### Launch REPL
```
moa
```

Intaractive console
```
> 1 + 2
3
```

### Compile
```
echo 'main: pr "Hello world"' > main.moa
moa build
./a
```

Output
```
Hello world
```

### Deployment
```
moa deploy localhost
```

Output
```
deploying to ssh://localhost:~/app/
```



## Create a new project for api server
```
$ moa new api
$ tree .
┣ .gitignore
┣ main.moa
┣ test.moa
┗ static/
  ┗ favicon.ico
```

You can see created files.
```# .gitignore
/a
/*.db
```

```# main.moa
record scheme:
  counter int

def main io:
  {request response} <- io.http.listen
  db <- io.db(scheme)
  if:
    request.path == "/":
      response.html "count " db.counter " <a href=/up id=up>up</a>"
    request.path == "/up":
      db.counter += 1
      response.redirect("/")
    response.file("static" request.path)
```

```# test.moa
test t:
  db <- t.db(scheme)
  b <- t.browse("/")
  t.eq(0 db.counter)
  b.has("count 0")

  b.click("#up")
  b.has("count 1")
  t.eq(1 db.counter)
```

Execute the program, then you can access `http://localhost:8000`.
```
moa run
```

```
Listen http://localhost:8000
```

Run test
```
moa test
```



## Manual for moa command
```
Usage:
  moa <command> [arguments]

The commands are:
  moa                   # launch REPL
  moa build [os] [arch] # compile to executable file
  moa deploy [target]   # deploy to serer
  moa new [template]    # create a new project
  moa run               # run the program
  moa test [path] ...   # run tests
```
