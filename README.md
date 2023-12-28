# The Moa Programming Language
Build robust web application rapidly
- Embedded components to enhance development experience
- An open-source programming language

Embedded components
- HTTP server
- Transactional database
- WebSocket
- Logging

Developer tools
- REPL
- Debugger
- Profiler



# Getting started

1. Install moa
```
$ mkdir -p ~/moa/bin
$ curl https://github.com/moa/bin/moa > ~/moa/bin/moa
$ export PATH=$PATH:~/moa/bin
```

2. At the command prompt, create a new Moa program
```
$ echo 'main io = io.puts "Hello, Moa"' > main.moa
```

3. Run the program
```
$ moa run
Hello, Moa
```

4. Compile the program to executable file
```
$ moa build
$ ./a.out
Hello, Moa
```

5. ToDo web application example
- index.mhtml
- main.moa

```
# index.mhtml
<!doctype html>
<html>
<head>
  <title>Access counter</title>
</head>
<body>
  <iframe src=/api/counter></iframe>
</body>
</html>
```

```
# main.moa
schema = access_counter:int

main io =
  req <- io.http.listen
  db <- io.db schema
  n = db.access_counter += 1
  req.ok n.string
```

6. Deploy to Linux server
```
$ OS=linux ARCH=amd64 moa build
$ scp a.out username@hostname:/path/to/a.out
$ ssh username@hostname /path/to/a.out upgrade # launch deployed server and then gracefully stop old one if running
```



7. Manual for moa command
Moa is a tool for managing Moa source code.

Usage:
  moa <command> [arguments]

The commands are:
  moa               # launch repl
  moa build         # compile to an optimized executable file
  moa run           # run Moa program
  moa test [regexp] # run tests
  moa version       # print Moa version
