# TODO
- [ ] infer: design default generic syntax
- [ ] infer: design constructor of type in name space
- [x] infer: generic and non generic type variable
- [ ] infer: variadic arguments
- [ ] infer: implement tnum for int, float
- [ ] infer: optimize constant int and float
- [ ] genjs
- [x] if / else
- [x] regexp literal
- [x] assert
- [x] struct
- [x] each
- [x] while
- [x] iif
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
{
  a
  b c
} -> (do a (b c))
```
