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
a b     >> (a b)
a()     >> (a)
a(b)    >> (a b)
!a      >> (! a)
a + b   >> (+ a b)
a.b     >> (. a b)
a[b]    >> ((. a at) b)
a -> b  >> (fn a b)
a,b ->
   a()
   b()  >> (fn a b (do (a) (b)))
{
  a
  b c
} -> (do a (b c))
```
