# TODO
- [x] Implement minimal code
- [ ] generate to C
- [ ] selfboot

# Syntax
```
atom:
| '"' ([^"\\]|\\.)* '"'
| [^ \n]+
| '(' atom* ')'
```

# Syntax sugar
```
a b   -> (a b)
a()   -> (a)
a(b)  -> (a b)
!a    -> (! a)
a + b -> (+ a b)
a.b   -> (. a b)
a[b]  -> ((. a [) b)
[a b] -> ([ a b])
{
  a
  b c
} -> (do a (b c))
```
