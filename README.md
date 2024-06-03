# The Moa Programming Language
Moa is an open source programming language that enhances your development experience for all development environments.



## Why Moa?
If some of the following are bothering you, Moa can help.

- Long waits to verify code changes  
  → Hot-loading
- Struggle with debugging  
  → Highly flexible debugger
- Technical dept and legacy code  
  → Enhance each part without affecting the whole



## How to enhance?
Moa is independent of specific operating system and programming langauges, but also provides numerous unique features to enhance your productivity:
- Allows modifying and running code on the fly
- Visualizes function calling and their side effects, including the time consumed
- Generates codes in JavaScript and so on(in the future...)



## Getting started


### Install
```
bash -c "$(curl -fsS https://github.com/moalang/moa/install.sh)"
exec $SHELL
```


### REPL
```
moa
```

Input
```
inc a:
  a + 1
inc 2
```

Output
```
3
```

Trace
```
trace
```

Output
```
inc 2   # 3
inc a:  # 2
  a + 1 # 2 -> 3
```


### Generate and execute code
```
echo 'inc a: a + 1' > main.moa
moa js
```

Output
```
function inc(a) {
  return a;
}
```



## Manual for moa command
```
Usage:
  moa <commands> [arguments]

The commands are:
  moa    # launch REPL
  moa js # print JavaScript
```
