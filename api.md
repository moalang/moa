# reserved
[ ] _           :: tuple[]
[ ] true, false :: bool
[ ] int         :: string option[int]
[ ] float       :: string option[float]
[ ] string a    :: a string
[ ] bool a      :: a bool
[ ] list a      :: a+ list[a]
[ ] dict a b    :: (a,b)+ dict[a b]
[ ] set a       :: a+ set[a]
[ ] tuple ...   :: tuple[...]
[ ] throw a b   :: a b
[ ] catch a b   :: a matcher[a b]+ b
[ ] match a b   :: a matcher[a b]+ b
[ ] num         # int, float

# embedded a
[ ] string :: string
[ ] format :: string string
[ ] bool :: bool

# num.embedded a
[ ] + - * ** / % | & ^ :: a a a
[ ] abs, neg :: a

# int.num
[ ] char   :: string
[ ] times  :: list[int]
[ ] to     :: int list[int]

# float.num
[ ] floor  :: int
[ ] ceil   :: int
[ ] round  :: int

# string
[ ] ++       :: string string string
[ ] size     :: int
[ ] pos      :: int
[ ] get      :: int option[string]
[ ] slice    :: int int? string
[ ] split    :: string int? list[string]
[ ] index    :: string int
[ ] replace  :: string string int? string
[ ] reverse  :: string
[ ] utf8     :: list[int]
[ ] find     :: string list[string]
[ ] rsplit   :: string int? list[string]
[ ] rreplace :: string fn[list[string] string] int? string

# lambda[a b ...]

# list[a]
[ ] ++ a :: list[a] list[a]
[ ] size :: int
[ ] get :: int a @error
[ ] set :: int a a @error
[ ] map b :: fn[a b] list[b]
[ ] fmap b :: fn[a list[b]] list[b]
[ ] keep :: fn[a bool] list[a]
[ ] all  :: fn[a bool] bool
[ ] any  :: fn[a bool] bool
[ ] slice :: int int? list[a]
[ ] sort b :: fn[a b]? list[a]
[ ] count b :: fn[a b]? dict[b int]
[ ] group b :: fn[a b]? dict[b list[a]]
[ ] reverse :: list[a]
[ ] zip b :: list[b] list[tuple[a b]]
[ ] find :: fn[a bool] option[a]
[ ] fold b :: fn[a b b] b
[ ] has :: a bool
[ ] sum :: a
[ ] min :: a
[ ] max :: a

# dict[k v]
[ ] size :: int
[ ] get :: k option[v]
[ ] set :: k v v
[ ] has :: k bool
[ ] keys :: seq[k]
[ ] values :: seq[v]
[ ] list :: list[tuple[k v]]

# set[a]
[ ] size :: int
[ ] - :: set[a] set[a]
[ ] | :: set[a] set[a]
[ ] & :: set[a] set[a]
[ ] ^ :: set[a] set[a]
[ ] has :: a bool

# tuple[a b ...]
[ ] 0 :: a
[ ] 1 :: b
...

# ---( pending )---------------------------------------
interface num a:
  + - * ** / / % :: a a a
  abs :: a
  neg :: a

implement int num:
  + a b: ...

# primitive
[ ] bytes       :: a bytes
[ ] seq         # string, list, dict, bytes

# statement
[ ] if a        :: bool a _
[ ] else a      :: a _
[ ] for a       :: .int int int? int? a _
[ ] each a b    :: .a seq[a] b _
[ ] while a     :: bool a _
[ ] continue    :: _
[ ] break       :: _
[ ] return a    :: a a

# bytes
[ ] ++ :: bytes bytes bytes
[ ] be :: bytes # big endian mode
[ ] le :: bytes # little endian mode
[ ] get :: int int @error
[ ] set :: int int @error
[ ] to a :: option[a]
[ ] write a :: a int
[ ] size :: int
[ ] slice :: int int? bytes
[ ] base64 :: string
[ ] string :: string

# math
[ ] acos acosh asin asinh atan atan2 atanh cbrt cos cosh erf erfc exp gamma log log10 log2 sin sinh sqrt tan tanh :: float float
[ ] e, pi, inf, nan :: float
[ ] hypot, logn :: float float float
[ ] lgamma, frexp :: float (float, int)
[ ] ldexp :: float int float

# log
[ ] debug, info, warn, error a :: a any* a

# time
[ ] year, month, day, hour, minute, second, wday, mday, yday :: int
[ ] time.now

# random
[ ] random.int :: int int int
[ ] random.bytes :: int bytes
[ ] random.choice a :: list[a] option[a]

# io.http
[ ] listen string (http.request http.response) _
[ ] request string {method.string headers.list[tuple[string list[string]]] body.bytes}? http.response

# io.db (object database management system)
[ ] begin a b :: a (b) b
[ ] readonly a b :: a (b) b
