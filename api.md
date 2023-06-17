# global
[ ] true, false :: bool
[ ] error :: any any
[ ] any a :: any(a)
[ ] int a :: a int @error
[ ] float a :: a float @error
[ ] string a :: a string
[ ] bool a :: a bool
[ ] bytes a :: a bytes
[ ] list a :: a+ list(a)
[ ] dict a b :: (a b)+ dict(a b)
[ ] set a :: a+ set(a)
[ ] tuple * :: tuple(*)
[ ] try a :: a try(a)
[ ] guard :: bool void @error
[ ] if / else
[ ] for / while / continue / break
[ ] return a :: a a

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
[ ] includes :: a bool

# tuple(a b ...)
[ ] 0 :: a
[ ] 1 :: b

# try(a)
[ ] then b :: (a b) try(b)
[ ] catch b :: (error b) b

# math
[ ] acos acosh asin asinh atan atan2 atanh cbrt cos cosh erf erfc exp gamma log log10 log2 sin sinh sqrt tan tanh :: float float
[ ] e, pi, inf, nan :: float
[ ] hypot, logn :: float float float
[ ] lgamma, frexp :: float (float, int)
[ ] ldexp :: float int float



---( effect )----------------------------------------------
# log
[ ] debug, info, warn, error a :: a any* a

# time
[ ] year, month, day, hour, minute, second, wday, mday, yday
[ ] time.now

# random
[ ] random.int :: int int int
[ ] random.string :: string string string
[ ] random.choice a :: list(a) a @error

# http
[ ] listen string (http.request http.response) ?
[ ] request string {method headers body} http.response

# odb (object database management system)
[ ] schema a :: odb.schema(a)
[ ] transaction a :: odb.schema(a) int @error
