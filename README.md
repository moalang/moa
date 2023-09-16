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
  moa format file    # format a file
  moa help [topic]   # for more info about topic
  moa lint file      # report likely mistakes
  moa run [exp]      # run Moa program
  moa test [regexps] # run tests
  moa version        # print Moa version



# Scafold for web application in real world
- index.mhtml
- main.moa

```
# index.mhtml
<!doctype html>
<html>
<head>
  <title>{{title}}</title>
</head>
<body>
  <h1>ToDo list</h1>
  <if todos>
    <ul>
      <for todos>
        <li>
          <if done>
            <s>{{ title }}</s>
          <else>
            {{ deadline }} {{ title }}
          </if>
      </for>
    </ul>
  <else>
    <p>All done</p>
  </if>

  <h1>New ToDo</h1>
  <form method="post" action="/todos">
    <input name="title" type="text">
    <input name="deadline" type="datetime-local">
    <label><input name="done" type="checkbox"> Done</label>
    <button>Create new ToDo</button>
  </form>
</body>
</html>
```

```
# main.moa

struct todo:
  title string
  deadline time
  done bool

struct schema:
  todos list[todo]

def main io:
  io.http.listen peer => io.database[schema] db =>
    def post_todos title deadline done:
      db.todos.add {title deadline done}
    match $"{peer.method} {peer.path}":
      "post /todos": post_todos peer.post("title") peer.post("deadline").time peer.post("done").bool
      _            : 404, [], "404 page not found"
```
