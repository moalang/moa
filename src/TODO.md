# TODO
- [ ] bootstrap with debugger
- [ ] selfboot to generate JavaScript
- [ ] selfboot to generate C

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
a[b]    -> ((. a [) b)
[a b]   -> ([ a b])
{
  a
  b c
} -> (do a (b c))
```
