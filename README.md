# The Moa Programming Language
Moa is an open source programming language that enhances your development experience.



## Why Moa?
If you have any frustrations, Moa can help you solve them.
For example:
- Runtime type error  
  →Static type check, type annotation is optional
- Slow tests  
  →Parallel fast tests
- Duplicated logic across applications  
  →Write once, generate different programming language code



## Getting started

Install
```
bash -c "$(curl -fsS https://raw.githubusercontent.com/moalang/moa/main/bin/install.sh)" && exec $SHELL
```

Hello world
```
echo '"Hello world"' | moa
```

```
Hello world
```

Access to web IDE
```
moa ide
listen http://127.0.0.1:3000
```



## Example: HTTP server with Go

handle.moa
```
struct request:
  path string

struct response:
  body string

def handle req:
  response req.path.slice(1)

test t:
  def eq expect path:
    t.eq expect handle(request(path)).body
  eq "" "/"
  eq "path/to" "/path/to"
```

main.go
```
package main

import "net/http"

func main() {
  http.ListenAndServe("127.0.0.1:3000", http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
    w.Write([]byte(handle(Request{path: r.URL.Path}).body))
  }))
}
```

Compile Moa to Go
```
moa to go
```

moa.go (generated)
```
package main

type request struct {
	path string
}

type response struct {
	body string
}

func handle(req request) response {
	return response{body: req.path[1:]}
}
```

moa_test.go (generated)
```
package main

import "testing"

func TestHandle(t *testing.T) {
	if handle(request{path: "/"}).body != "" {
		t.Fatal("not equal")
	}
	if handle(request{path: "/path/to"}).body != "path/to" {
		t.Fatal("not equal")
	}
}
```

Run test
```
go test moa.go moa_test.go
```

```
ok  	command-line-arguments	0.120s
```

Start the server
```
go run
```

Request to the server
```
curl http://localhost:3000/hello
```

Output
```
hello
```



### Example: HTTP server with Node

handle.moa
```
struct request:
  path string

struct response:
  body string

def handle req:
  response req.path.slice(1)

test t:
  def eq expect path:
    t.eq expect handle(request(path)).body
  eq "" "/"
  eq "path/to" "/path/to"
```

main.mjs
```
import { handle } from './moa.mjs'
import { createServer } from 'node:http'
createServer((req, res) => res.end(handle({path: req.url}).body)).listen(3000)
```

Compile Moa to mjs
```
moa to mjs
```

moa.mjs (generated)
```
export const request = path => ({path})
export const response = body => ({body})
export const handle = req => response(req.path.slice(1))
```

moa.test.js (generated)
```
import test from 'node:test'
import assert from 'node:assert'
import { request, response, handle } from './moa.mjs'

test('handle.moa', t => {
  assert.strictEqual('', handle(request('/')).body)
  assert.strictEqual('path/to', handle(request('/path/to')).body)
})
```

Run test
```
node --test
```

```
✔ handle.moa (0.561792ms)
ℹ tests 1
ℹ suites 0
ℹ pass 1
ℹ fail 0
ℹ cancelled 0
ℹ skipped 0
ℹ todo 0
ℹ duration_ms 56.09625
```

Start the server
```
node main.mjs
```

Request to the server
```
curl http://localhost:3000/hello
```

Output
```
hello
```



### For JavaScript in web browser
add.moa
```
def add a b:
  a + b

test t:
  t.eq 3 add(1 2)
```

Compile to JavaScript
```
moa to mjs
```

moa.mjs (generated)
```
export const add = (a, b) => a + b
```

moa.test.mjs (generated)
```
import test from 'node:test'
import assert from 'node:assert'
import { add } from './moa.mjs'

test('add.moa', t => {
  assert.strictEqual(3, add(1, 2))
})

```

Test
```
node --test
```

```
✔ add.moa (0.524792ms)
ℹ tests 1
ℹ suites 0
ℹ pass 1
ℹ fail 0
ℹ cancelled 0
ℹ skipped 0
ℹ todo 0
ℹ duration_ms 53.017208
```




## Moa command usage
```
Usage:
    moa                       # launch interactive shell
    moa env [+/-] [<version>] # list versions; use, install or remove a version
    moa ide [<port>]          # launch web IDE
    moa to [<language>]       # compile to a programming language

The languages are:
    go                        # generate moa.go and moa_test.go
    mjs                       # generate moa.mjs and moa_test.mjs
```


## Interactive shell commands 
```
:             -- repeat last command
:q            -- quit the shell
:u <language> -- use a programming language
```
