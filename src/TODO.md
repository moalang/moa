# TODO
- [ ] infer: design default generic syntax
- [ ] infer: design constructor of type in name space
- [x] infer: generic and non generic type variable
- [x] infer: variadic arguments -> only embedded vec, map
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
- [x] bool
- [x] int
- [x] float
- [x] string
- [x] bytes
- [x] regexp
- [x] fn
- [x] let
- [ ] dec
- [ ] class
- [ ] enum
- [ ] unary operator
- [ ] binary operator
- [ ] iif
- [ ] throw
- [ ] catch
- [ ] do
- [ ] for
- [ ] while
- [ ] continue
- [ ] break
- [ ] goto
- [ ] opt: alias to enum
- [ ] vec: alias to class
- [ ] set: alias to class
- [ ] map: alias to class
- [ ] tuple: alias to class
- [ ] def: alias to let + fn

# Core Syntax
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

# Core feature
- literal: bool, int, float, string, bytes, regexp, fn
- let, dec, class, enum
- unary operator
- binary operator
- iif, throw, catch
- do, for, while, continue, break, goto

# Alias feature
- case
- tuple, opt, set, vec, dict
