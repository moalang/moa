# The Moa Programming Language
Moa is a general purpose programming language for simple and robust software.
- An open-source programming language
- Simple grammer

Developer tools
- REPL
- Compiler
- Integration for C, JavaScript



# Getting started

1. Installation for Mac and Linux
```
bash -c "$(curl -fsS https://github.com/moalang/moa/install.sh)"
exec $SHELL
```

2. Launch REPL
```
$ moa
Moa 0.0.1
> 1 + 2
3
```

```
$ echo 'def add a b: a + b' > main.moa
$ moa
Moa 0.0.1
> add 2 3
5
```

3. Compile the program
```
$ echo 'def main io: io.puts "Hello world"' > main.moa
$ moa build
$ ./a
Hello world
```

```
$ moa build linux amd64 # a
$ moa build js          # moa.js
$ moa build c           # moa.c
```

4. Manual for moa command
```
Usage:
  moa <command> [arguments]

The commands are:
  moa                    # launch REPL
  moa build [option] ... # compile
  moa test [path] ...    # run tests
  moa format [path] ...  # format code
```
