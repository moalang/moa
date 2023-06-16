# global
[ ] true, false :: bool
[ ] error :: any any
[ ] try a :: a option(a)
[ ] any a :: any(a)

# bool
[ ] flip :: bool

# int
[ ] / :: int int float
[ ] + - * ** // % :: int int int
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
[ ] slice :: int int string
[ ] split :: string int? list(string)
[ ] index :: string int @error
[ ] replace :: string string int? string
[ ] reverse :: string
[ ] ord :: int
[ ] match :: string list(string)
[ ] rsplit :: string list(string)
[ ] rreplace :: string (list(string) string) int? string
[ ] bool :: bool

# bytes
[ ] ++ :: bytes bytes bytes
[ ] size :: int
[ ] int :: int int int
[ ] float :: int int float
[ ] bytes :: int int bytes
[ ] string :: int int string @error
[ ] write :: any int @error
[ ] at :: int int @error

# list(a)
[ ] ++ a :: list(a) list(a) list(a)
[ ] size :: int
[ ] at :: int a @error
[ ] map b :: (a b) list(b)
[ ] filter :: (a bool) list(a)
[ ] slice :: int int list(a)
[ ] sort b :: (a b)? list(a)
[ ] reverse :: list(a)
[ ] zip b :: list(b) list(tuple(a b))
[ ] find :: (a bool) a @error
[ ] bool :: bool

# dict(k v)
[ ] size :: int
[ ] at :: k v @error
[ ] keys :: list(k)
[ ] values :: list(v)
[ ] filter :: (k v bool) dict(k v)
[ ] list :: list(tuple(k v))
[ ] find :: (k v bool) tuple(k v) @error
[ ] bool :: bool

# set(a)
[ ] size :: int
[ ] - :: set(a) set(a)
[ ] | :: set(a) set(a)
[ ] & :: set(a) set(a)
[ ] includes :: a bool
[ ] bool :: bool

# tuple(a b ...)
[ ] 0 :: a
[ ] 1 :: b

# option(a)
[ ] then b :: (a b) option(b)
[ ] catch b :: (error b) b
[ ] success :: bool

# math
[ ] acos acosh asin asinh atan atan2 atanh cbrt cos cosh erf erfc exp gamma log log10 log2 sin sinh sqrt tan tanh :: float float
[ ] e, pi, inf, nan :: float
[ ] hypot, logn :: float float float
[ ] lgamma, frexp :: float (float, int)
[ ] ldexp :: float int float



---( effect )----------------------------------------------
# log
[ ] debug, info, warn, error :: any+ any

# time
[ ] year, month, day, hour, minute, sec, wday, mday, yday
[ ] time.now

# random
[ ] int :: int int int
[ ] string :: string string string
[ ] choice a :: list(a) a @error

# http
[ ] listen string (http.request http.response) ?
[ ] request string {method headers body} http.response

# odb (object database management system)
[ ] schema a :: odb.schema(a)
[ ] transaction a :: odb.schema(a) int @error
