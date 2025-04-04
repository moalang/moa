# Light weight syntax is 30 percent brief than only lisp style

> dec compile: str str
compile:: str str

> def tokenize moa:
tokenize moa =

> let c = moa[i]
c = moa[i]

> indent = 1
indent := 1

> a + b + c
++ a b c

iif a:
  b
  c

iif:
  a: 1
  b: 2
  3

match root:
  node [leaf(t) ...r]: ++ gen(node(a)) "(" r.map(gen).join(", ") ")"
