# TODO
[ ] define a type of block syntax
[ ] define a interface for iterator

# global core
[ ] true, false :: bool
[ ] nil :: tuple()
[ ] int a :: string int @error
[ ] float a :: string float @error
[ ] string a :: a string
[ ] bool a :: a bool
[ ] bytes a :: a bytes
[ ] list a :: a+ list(a)
[ ] dict a b :: (a b)+ dict(a b)
[ ] set a :: a+ set(a)
[ ] tuple ... :: tuple(...)
[ ] throw a :: a a
[ ] if a :: bool a nil
[ ] else a :: a nil
[ ] for :: ID:int int int? int? block nil
[ ] for a :: ID:a iterator(a) block nil
[ ] while a :: a block nil
[ ] continue :: nil
[ ] break :: nil
[ ] return a :: a a
[ ] i8, i16, i32, i64, u8, u16, u32, u64, f32, f64
[ ] num a # int, float, i8, i16, i32, i64, u8, u16, u32, u64, f32, f64
[ ] iterator a
[ ] block :: (bool) # break returns false otherwise true (including continue)

# bool
[ ] flip :: bool

# int
[ ] / :: int int float
[ ] + - * ** // % | & ^ :: int int int
[ ] abs :: int
[ ] neg :: int
[ ] char :: string

# float
[ ] + - * ** / // % :: float float float
[ ] abs :: float
[ ] neg :: float
[ ] floor :: int
[ ] ceil :: int

# string
[ ] ++ :: string string string
[ ] size :: int
[ ] pos :: int
[ ] at :: int string @error
[ ] slice :: int int? string
[ ] split :: string int? list(string)
[ ] index :: string int @error
[ ] replace :: string string int? string
[ ] reverse :: string
[ ] ordinal :: int
[ ] match :: string list(string)
[ ] rsplit :: string list(string)
[ ] rreplace :: string (list(string) string) int? string

# bytes
[ ] ++ :: bytes bytes bytes
[ ] be :: bytes # change to big endian as default
[ ] le :: bytes # change to little endian
[ ] read a :: a @error
[ ] write a :: a int @error
[ ] size :: int
[ ] slice :: int int? bytes
[ ] base64 :: string
[ ] string :: string string # .string("A-Za-z0-9+/") is almost same as .base64

# lambda(a b ...)
[ ] string :: string

# list(a)
[ ] ++ a :: list(a) list(a) list(a)
[ ] size :: int
[ ] at :: int a @error
[ ] map b :: (a b) list(b)
[ ] filter :: (a bool) list(a)
[ ] slice :: int int? list(a)
[ ] sort b :: (a b)? list(a)
[ ] reverse :: list(a)
[ ] zip b :: list(b) list(tuple(a b))
[ ] find :: (a bool) a @error
[ ] fold b :: (a a b) b
[ ] sum :: a

# dict(k v)
[ ] size :: int
[ ] at :: k v @error
[ ] keys :: list(k)
[ ] values :: list(v)
[ ] filter :: (k v bool) dict(k v)
[ ] list :: list(tuple(k v))
[ ] find :: (k v bool) tuple(k v) @error

# set(a)
[ ] size :: int
[ ] - :: set(a) set(a)
[ ] | :: set(a) set(a)
[ ] & :: set(a) set(a)
[ ] ^ :: set(a) set(a)
[ ] includes :: a bool

# tuple(a b ...)
[ ] 0 :: a
[ ] 1 :: b
...

---( interface )---------------------------------------
interface num a:
  / :: a a float
  + - * ** // % :: a a a
  negate, abs, signum :: a

class iterator a:
  fn next: ...

interface iterator m a:
  next a?
  # for the following methods, you can override default implementations
  map b :: (a b) m(a)
  fmap b :: (a m(b)) m(b)
  filter :: (a bool) m(a)
  ++ :: m(a) m(a)
  slice :: int int? m(a)
  each_slice :: int m(m(a))
  group_by b :: (a b) dict(b m(a))
  reverse :: m(a)
  sort :: m(a)
  sort_by b :: (a b) m(a)
  drop_while :: (a bool) m(a)
  take_while (a bool) m(a)
  zip b :: m(b) m((a,b))

  head :: a?
  fold b :: (a b b) b b
  max :: a?
  min :: a?
  minmax :: (a,a)?
  sum :: a
  size :: int

  present :: bool
  empty :: bool
  all :: (a bool) bool
  any :: (a bool) bool
  include :: a bool

  count_by b :: (a b)? dict(b int)
  list :: list(a)


---( module )------------------------------------------
# math
[ ] acos acosh asin asinh atan atan2 atanh cbrt cos cosh erf erfc exp gamma log log10 log2 sin sinh sqrt tan tanh :: float float
[ ] e, pi, inf, nan :: float
[ ] hypot, logn :: float float float
[ ] lgamma, frexp :: float (float, int)
[ ] ldexp :: float int float



---( io )----------------------------------------------
# log
[ ] debug, info, warn, error a :: a any* a

# time
[ ] year, month, day, hour, minute, second, wday, mday, yday
[ ] time.now

# random
[ ] random.int :: int int int
[ ] random.bytes :: int bytes
[ ] random.choice a :: list(a) a @error

# http
[ ] listen string (http.request http.response) ?
[ ] request string {method headers body} http.response

# odb (object database management system)
[ ] schema a :: odb.schema(a)
[ ] transaction a :: odb.schema(a) int @error
