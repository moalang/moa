# The Moa Programming Language
Moa is an open source programming language that enhances your development experience.



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
  io.http.serve ":8000" req => {text "hello"}

test t:
  t.main()
  t.get("/").body.eq("hello")
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



## Example: API server

http.moa
```
struct schema:
  todos    vec[todo]

struct todo:
  id         int
  title      string
  done       bool
  created_at time

def main:
  req <- io.serve ":8000"
  db <- io.db[schema] "db.jsonl"
  match req.route:
    "get /todos":
      new json db
    "post /todos":
      db.todos.insert req{title done created_at=io.now()}
    new status 400 text "not found"
```

```
moa run http.moa
```

```
curl -d "title=walking" http://localhost:8000/todos
curl http://localhost:8000/todos
```

```
{"todos": [{"id":1,"title":"walking","created_at":"2000-01-02T03:04:05Z"}]}
```

```
cat db.jsonl
```

```
{"todos": [{"id":1,"title":"walking","created_at":"2000-01-02T03:04:05Z"}]}
```

## Example: API server for single page application

http.moa
```
struct schema:
  users    vec[user]
  projects vec[project]
  sessions map[string int]

struct user:
  id       int
  email    string
  pwhash   bcrypt
  reads    set[int]
  writes   set[int]

struct project:
  id    int
  name  string
  todos vec[todo]

struct todo:
  id    int
  title string
  done  bool

def main:
  req <- io.serve ":8000"
  db <- io.db[schema] "db.jsonl"
  let pid = req.query("pid").int
  match req.route:
    "get /":
      new file "index.html"

    "post /api/signin":
      if user = db.users.find(u => u.email == req.post("email")) && user.pwhash.eq(req.post("password")):
        let sid = io.random.bytes(24).base64()
        db.sessions[sid] = user.id
        return : new 200 json user{id email} cookie("sid" sid)
      new 200 json map

    if user = db.sessions[req.cookie("sid")].then(db.users) && user.reads.has(pid):
      "get /api/ws":
        new subscribe pid

    if user.writes.has(pid):
      "get /api/project":
        new json db.projects[pid]

      "patch /api/project":
        let patch = db.mutate(req.text()).json()
        new status 204 publish(pid patch)

    new status 400 text "not found"

test t:
  t.main()
  db <- t.db
  db.users.insert : new email "a@a.com" pwhash bcrypt("password")
  db.projects.insert : new name "project 1"
  let mutation = `insert todo title "todo 1"`

  t.eq 200 t.get("/").status
  let sid = t.post("/api/signin" "email" "a@a.com" "password").status.eq(200).cookie("sid")
  ws <- t.get("/api/ws?pid=1" (new cookie sid)).ws()
  t.eq `{"id": 1, "name": "project 1"}` t.get("/api/project/?pid=1").text()
  t.eq 204 t.patch("/api/project" mutation).status
  t.eq `{"id": 1, "name": "project 1", "todos": [{id: 1, title: "todo 1"}]}` t.get("/api/project/?pid=1").text()
  t.eq (vec mutation) ws.messages

  # no authentication
  t.get("/api/ws").status.eq(404)
  t.get("/api/project").status.eq(404)
  t.patch("/api/project").status.eq(404)
```
