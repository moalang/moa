# TODO
- [ ] Implement minimal code
- [ ] Show call tree in IDE
- [ ] Show variables at a selected line in IDE
- [ ] Show trace graph in IDE
- [ ] generate to JavaScript
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
