# Build-in types
Primitive
- bool
- int
- float
- string
Container
- tuple
- class
- array
- dict
- set
- enum
Interface
- void
- error
- function
- number
- try
Control flow
- effect?
- branch
Binary operators
- effect      : <-
- comparision : == != >= <= > < || &&
- update      : += -= *= /= %=
- array       : ++
- string      : .
- number      : + - * / % **
- available   : @ & &&& ||| //
- reserved    : | , ! ? ^



# Core library
bool:
  guard : string try(void)
int:
  to_s : try(string)
float:
  to_s : try(string)
string:
  join : string string
  split : string string
  to_i : try(int)
  to_f : try(float)
  to_a : string.to_a
function a b c:
  name : string
  bind a : function(b c)
turple a b:
  n0 : a
  n1 : b
array a:
  reduce b : function(a b) b
  map b : b.array
  filter : function(a bool) a.array
  include : a bool
  to_set : a.set
map k v:
  keys : k.array
  values : v.array
set a:
  to_array : a.array
enum:
  keys : string.array
  count : int
number a:
  +, -, *, / : a a a
try a:
  and : a.try a.try
  or : a.try a.try
  fmap b : function(a b) try.b
  then b : function(error try.b) try.b
log:
  debug a : a void
  info a : a void
  warn a : a void
  error a : a void
