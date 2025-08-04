# TODO
- [x] infer: design default generic syntax -> fn[1 1](x x), fn[t.num t t t](a b a + b)
- [x] infer: design constructor of type in name space -> embeded types, class and enum are specialized
- [x] infer: generic and non generic type variable
- [x] infer: variadic arguments -> only embedded vec, map
- [ ] infer: implement number interface to int and float for constant integer
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
- let, class, enum
- unary operator
- binary operator
- iif, throw, catch
- do, for, while, continue, break, goto

# Alias feature
- case
- tuple, opt, set, vec, dict
