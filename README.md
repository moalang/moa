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

4. Compile the program to executable file
```
$ moa build
$ ./a.out
Hello, Moa

$ OS=windows ARCH=amd64 moa build
$ ls a.exe
a.exe
```

5. Compile the program to anotner programming langauge
```
$ moa compile to 
$ ./a.out
Hello, Moa
```

5. Create a web application and deploy to a server
```
$ echo 'def main io: io.listen s => s.puts "hello"' > main.moa
$ PORT=3000 moa run        # you can check the behavior in local
$ moa deploy user@hostname # deploy to ~/moa on the server and update the process non-stopping
```



# Usage
Moa is a tool for managing Moa source code.

Usage:
  moa <command> [arguments]

The commands are:
  moa                # launch repl
  moa build          # compile to a executable file
  moa format [files] # format files
  moa help [topic]   # for more info about topic
  moa lint [files]   # report likely mistakes
  moa run [exp]      # run Moa program
  moa test [regexps] # run tests
  moa version        # print Moa version
