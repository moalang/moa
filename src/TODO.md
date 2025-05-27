# TODO
- [ ] selfboot
- [ ] moa repl
- [ ] moa test
- [ ] bool   methods
- [ ] int    methods
- [ ] float  methods
- [ ] string methods
- [ ] bytes  methods
- [ ] tuple  methods
- [ ] vec    methods
- [ ] map    methods
- [ ] set    methods
- [ ] moa ide
- [ ] io.random
- [ ] io.file
- [ ] io.db
- [ ] io.http
- [ ] io.bcrypt
- [ ] moa env
- [ ] package manager for Moa
- [ ] io.window

# Syntax
```
atom:
| '"' ([^"\\]|\\.)* '"'
| [^ \n]+
| '(' atom* ')'
```

# Syntax sugar
```
a b     -> (a b)
a()     -> (a)
a(b)    -> (a b)
!a      -> (! a)
a + b   -> (+ a b)
a.b     -> (. a b)
a[b]    -> ((. a at) b)
a =
  b
  c d   -> (= a (do b (c d)))
a => b  -> (fn a b)
a,b =>
   a()
   b()  -> (fn a b (do (a) (b)))

[TBD]
a: b c  -> (a (b c))
a:
  b
  c d   -> (a (do b (c d)))





let pi = 3.14
var counter = 0
def now = io.now()
def inc n = n + 1


```
