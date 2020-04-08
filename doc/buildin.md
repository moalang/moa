# Build-in types
Primitive
- bool
- int
- float
- string
Container
- tuple
- struct
- array
- dict
- enum
Interface
- void
- error
- function
- number
Control flow
- try
- effect
- branch
Binary operators
- effect      : <- += -= *= /= %=
- comparision : == != >= <= > < || &&
- array       : ++
- string      : .
- number      : + - * / % **
- available   : @ & &&& ||| //
- reserved    : | , ! ? ^

# Core library
any:
  to_s : string
bool:
  -
num:
  to_i
  to_f
  (+, -, *) : a a a
int:
  num
  // : a a a
float:
  num
  / : a a a
string:
  join : string string
  split : string string
  to_i : try(int)
  to_f : try(float)
  to_a : array(string)
func a b c:
  name : string
  code : string
  curry a : func(b c)
turple a b:
  n0 : a
  n1 : b
array a:
  reduce b : function(a b) b
  map b : array(b)
  filter : function(a bool) array(a)
  has : a bool
dict k v:
  keys : array(k)
  values : array(v)
enum:
  keys : string(string)
  count : int
try a:
  and : a.try a.try
  or : a.try a.try
  fmap b : (a b) try.b
  catch b : (error try.b) try.b
eff a:
  try
log:
  debug: any void
  info: any void
  warn: any void
  error: any void
