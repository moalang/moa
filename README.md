# The Moa Programming Language
Moa is an open source programming language which helps programming for fun! 


# Getting started

Set up
```
mkdir -p ~/moa/bin
curl https://github.com/moa/bin/moa > ~/moa/bin/moa
export PATH=$PATH:~/moa/bin
```

Create a code
```
# echo 'def main: io.print "hello world"' > main.moa
```

Run
```
# moa run
hello world
```

Build
```
# moa build
# ./main
hello world
```

Test
```
# echo 'test t: t.eq "hi" t.main.stdout
# moa test
expect: hi
actual: hello world\n
```
