# The Moa Programming Language
Build simple and reliable system rapidly
- An open-source programming language
- Seamless development for web frontend and backend
- Built-in HTTP server, RDB, KVS, bidirectional RPC



# Getting started

1. Install moa
```
$ mkdir -p ~/moa/bin
$ curl https://github.com/moa/bin/moa > ~/moa/bin/moa
$ export PATH=$PATH:~/moa/bin
```

2. At the command prompt, create a new Moa program
```
$ echo 'def main io: io.puts "Hello, Moa"' > main.moa
```

3. Run the program
```
$ moa run
Hello, Moa
```

4. Create a HTTP API and deploy to a server
```
$ echo 'def main io: io.http.listen _ => (200 [("content-type" "text/plain; charset=utf-8")] "hello")' > main.moa
$ PORT=3000 moa run        # launch http server with port 3000
```

5. Compile the program to executable file
```
$ moa build
$ ./a.out
Hello, Moa
```

6. Deploy to Linux server
```
$ OS=linux ARCH=amd64 moa build
$ scp a.out username@hostname:/path/to/a.out
$ ssh username@hostname /path/to/a.out # launch deployed server and then gracefully stop old one if running
```



# Usage
Moa is a tool for managing Moa source code.

Usage:
  moa <command> [arguments]

The commands are:
  moa                # launch repl
  moa build          # compile to an executable file without test
  moa format [files] # format files
  moa help [topic]   # for more info about topic
  moa lint [files]   # report likely mistakes
  moa run [exp]      # run Moa program
  moa test [regexps] # run tests
  moa version        # print Moa version



# Scafold for web application in real world
/
|- static/index.html
|- static/style.css
|- static/script.js
-- main.moa

```
struct article:
  title string
  content string

db:
  pv int
  articles list[article]

# main.moa
def main io:
  io.http.listen handle

let notfound = (404 [] "")

def handle peer:
  db.pv += 1
  iif peer.path == "/api/articles" && peer.method == "get" json(db.articles) notfound

def json o:
  (200 ["content-type" "application/json; charset=utf-8" struct(o).json)
```
