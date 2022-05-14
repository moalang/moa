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
# echo 'p "hello world"' > main.moa
```

Run
```
# moa run hello.moa
hello world
```

Build
```
# moa build hello.moa > main
# ./main
hello world
```

Test
```
# echo '\ntest t: t.eq "hi" >> hello.moa
# moa test hello.moa
expect: hi
actual: hello world\n
```
