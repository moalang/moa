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
# main.moa
struct user:
  id int
  nickname string
  email string
  pw_hash string

var db:
  pv int
  users list[user]
  sessions dict[string user]
  ttl(sessions=30min)

def main io:
  def broadcast o:
    io.http.active_sockets.each(socket => socket.write(struct(o).json))
  io.http.websocket "/ws" socket => io.db.readonly db () => db.sessions.includes(socket.header("sid"))
  io.http.listen "/api/" peer => io.db.begin db () => dispatch peer broadcast
  io.http.static "static/"

def dispatch peer broadcast:
  def json o:
    (200 ["content-type","application/json; charset=utf-8"] struct(o).json)
  def login email password:
    db.users.find(u => u.email == email && password.eq_hash(u.pw_hash))

  db.pv += 1
  let user db.sessions[peer.header("sid")]
  match $"{peer.method} {peer.path}":
    "post /api/login": json login(peer.post("email") peer.post("password"))
    "get /api/uesrs" if user: json memory.users.map(u => u{id nickname})
    "post /api/send/chat" if user: json broadcast({message: peer.post("message")})
    _: peer.status404
```
