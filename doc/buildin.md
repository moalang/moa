# Build-in types
Primitive
- bool
- int         : `(+, -, *, **, ^, //)`, float
- float       : `(+, -, *, /)`        , int
- string      : join, split, int, float, array
Structure
- tuple       : n0, n1, ...
- struct
- enum
- func        : curry a b c ... func(b c ...)
Container
- array       : +seq
- dict        : +seq, keys, values
Interface
- any         : string, empty
- void
- error       : and, or
Control flow
- opt         : and, or
- try         : and, or
- do
- io          : and, or
Binary operators
- effect      : `<- += -= *= /= %=`
- comparision : `== != >= <= > < || &&`
- array       : `++`
- string      : `.`
- number      : `+ - * / % **`
- available   : `@ & &&& ||| //`
- reserved    : `| , ! ? ^`
System call
- io          : stdin.read, stdout, stderr
Type Class
- any a       : eq, order, string
- bounded a   : min, max
- seq a       : +functor, count, reduce, filter, has, array
- functor a   : map

# Core
io:
  stdin
  stdout
  stderr
log:
  debug: any void
  info: any void
  warn: any void
  error: any void
