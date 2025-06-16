# The Moa Programming Language
Moa is an open-source programming language designed to improve the developer experience.



## Getting started

Install
```
bash -c "$(curl -fsS https://raw.githubusercontent.com/moalang/moa/main/misc/install.sh)" && exec $SHELL
```

Hello world
```
echo 'def main: io.puts "Hello world"' > hello.moa
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
def main:
  io.http.serve ":8000" req => new text "hello"

test t:
  t.main()
  t.eq "hello" t.get("/").body
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
