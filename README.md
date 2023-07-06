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
$ echo 'def main c: c.puts "Hello, Moa"' > main.moa
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
$ echo 'def main c: c.listen "hello"' > main.moa
$ PORT=3000 moa run        # you can check the behavior in local
$ moa deploy user@hostname # deploy to ~/moa on the server and update the process non-stopping
```
