# The Moa Programming Language
Moa is an open source programming language that enhances your development experience.



## Getting started

Install
```
bash -c "$(curl -fsS https://raw.githubusercontent.com/moalang/moa/main/bin/install.sh)" && exec $SHELL
```

Hello world
```
echo 'main: puts "Hello world"' | moa
```

```
Hello world
```



## Example: HTTP server

http.moa
```
use http

main = http.serve "localhost:8000" req => (type:"text/plain" body:"hello")

test = assert.equal "hello" main.get("/").body
```

```
moa test
```

```
.ok
```

```
moa run
```

```
curl http://localhost:8000
```

```
hello
```



## Example: Static site generator
```
use fs

main = fs.files "/**/*.md" file => fs.write path html {
  path = file.path.replace ".md" ".html"
  html = util.md_to_html file.string
}

package util

md_to_html md = "html"
```



## Example: fibonatch
```
fib n =
  a = 0
  b = 1
  while a < n {
    yield a
    a, b = b, a + b
  }

main =
  puts fib(1000)
```



## Moa command usage
```
Usage:
    moa                   # launch interactive shell
    moa run  [--watch]    # run the program
    moa test [--watch]    # test the program
    moa build [os] [arch] # compile to executable file
```



## Interactive shell commands 
```
:          -- repeat last command
:q         -- quit the shell
```
