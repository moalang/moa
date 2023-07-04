# The Moa Programming Language
Moa is an open source programming language which helps programming for fun!

# Getting started

1. Install moa
```
$ mkdir -p ~/moa/bin
$ curl https://github.com/moa/bin/moa > ~/moa/bin/moa
$ export PATH=$PATH:~/moa/bin
```

2. At the command prompt, create a new Moa program
```
$ echo 'console c: c.puts "Hello, Moa"' > main.moa
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

5. Create a web application and deploy to a server
```
$ echo 'web c: c.puts "hello"' > main.moa
$ moa run                  # you can the behavior in local
$ moa deploy user@hostname # deploy to ~/moa on the server and update the process non-stopping
```
