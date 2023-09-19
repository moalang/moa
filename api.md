# reserved
[ ] _                  :: tuple[]
[ ] true, false        :: bool
[ ] string a           :: a string
[ ] bytes a            :: a bytes
[ ] bool a             :: a bool
[ ] error a b          :: a b
[ ] match a b          :: a matcher[a b]+ b
[ ] <,<=,==,!==,>,>= a :: a a bool
[ ] num                # int, float

# opt a
[ ] and b    :: opt[a] (a b) opt[b]
[ ] or       :: opt[a] a a
[ ] default  :: a
[ ] location :: string

# num a
[ ] + - * ** / % | & ^ :: a a a
[ ] abs, neg :: a
[ ] format :: string string

# int.num
[ ] char   :: string
[ ] time   :: time
[ ] times  :: list[int]
[ ] to     :: int list[int]

# float.num
[ ] floor  :: int
[ ] ceil   :: int
[ ] round  :: int

# string
[ ] ++          :: string string string
[ ] size        :: int
[ ] slice       :: int int? string
[ ] split       :: string int? list[string]
[ ] index       :: string int
[ ] replace     :: string string int? string
[ ] reverse     :: string
[ ] has         :: string bool
[ ] starts      :: string bool
[ ] ends        :: string bool
[ ] rsplit      :: string int? list[string]
[ ] rreplace    :: string (list[string] string) int? string
[ ] time        :: opt[time]
[ ] int         :: opt[int]
[ ] float       :: opt[float]
[ ] bcrypt      :: string
[ ] eq_bcrypted :: string bool

# bytes
[ ] ++              :: bytes bytes bytes
[ ] le              :: bytes         # switch to litle endian mode
[ ] i8,i16,i32,i64  :: int opt[int]
[ ] u8,u16,u32,u64  :: int opt[int]
[ ] f32,f64         :: int opt[int]
[ ] size            :: int
[ ] slice           :: int int? bytes
[ ] utf8,utf16,hex  :: string
[ ] tr              :: string string # b.tr("A-Za-z0-9+/") is same as base64
[ ] hash            :: int
[ ] shash           :: string bytes @error
[ ] hmac            :: bytes bytes string? bytes @error

# lambda[a b ...]

# list[a]
[ ] ++ a    :: list[a] list[a]
[ ] size    :: int
[ ] first   :: opt[a]
[ ] get     :: int opt[a]
[ ] []      :: int a @error
[ ] []=     :: int a a @error
[ ] map b   :: (a b) list[b]
[ ] fmap b  :: (a list[b]) list[b]
[ ] keep    :: (a bool) list[a]
[ ] all     :: (a bool) bool
[ ] any     :: (a bool) bool
[ ] slice   :: int int? list[a]
[ ] sort b  :: (a b)? list[a]
[ ] count b :: (a b)? dict[b int]
[ ] group b :: (a b)? dict[b list[a]]
[ ] reverse :: list[a]
[ ] zip b   :: list[b] list[tuple[a b]]
[ ] fold b  :: (a b b) b
[ ] find    :: (a bool) opt[a]
[ ] has     :: a bool
[ ] min     :: a
[ ] max     :: a

# dict[k v]
[ ] size   :: int
[ ] get    :: k opt[v]
[ ] set    :: k v bool
[ ] has    :: k bool
[ ] keys   :: list[k]
[ ] values :: list[v]
[ ] list   :: list[tuple[k v]]

# set[a]
[ ] size :: int
[ ] -    :: set[a] set[a]
[ ] |    :: set[a] set[a]
[ ] &    :: set[a] set[a]
[ ] ^    :: set[a] set[a]
[ ] has  :: a bool

# tuple[a b ...]
[ ] 0 :: a
[ ] 1 :: b
...

# time
[ ] year, month, day, hour, min, sec, wday, yday, offset, int :: int
[ ] format :: string string
[ ] zone   :: string
[ ] +      :: int time time
[ ] +      :: time int time
[ ] -      :: time time int

# ---( standard module )-------------------------------

# io
[ ] argv :: list[string]
[ ] put  :: string _ @error
[ ] puts :: string _ @error
[ ] gets :: string
[ ] stdin :: bytes

# ---( pending )---------------------------------------
# io
[ ] now  :: time
[ ] path string:
[ ]   path   :: string
[ ]   join   :: string path
[ ]   glob   :: string list[path]
[ ]   read   :: opt[bytes]
[ ]   write  :: bytes _ @error
[ ]   append :: bytes _ @error
[ ]   unlink :: @error
[ ] debug, info, warn, error a :: any* a a
[ ] database t u :: (t u) u
[ ] randint :: int int int
[ ] randbytes :: int bytes
[ ] http:
[ ]   listen (http.request http.response) _
[ ]   call string {method.string="get" headers.list[tuple[string list[string]]]=[] body.bytes=[]} http.response
[ ]   get url option.. = call url {method="get" option..}
[ ]   post url option.. = call url {method="post" option..}
[ ]   request:
        method  :: string
        path    :: string
        header  :: string string
        headers :: string list[string]
        get     :: string string
        gets    :: string list[string]
        post    :: string string
        posts   :: string list[string]
        body    :: bytes
[ ]   response:
        status  :: int
        headers :: list[tuple[string string]]
        body    :: bytes


# interface
eq = interface a:
  eq: a a bool
  ne l r = !eq l r

time.eq.eq l r = l.int == r.int

# statement
[ ] if a      :: bool a _
[ ] else a    :: a _
[ ] for a     :: .int int int? int? a _
[ ] each a b  :: .a seq[a] b _
[ ] while a   :: bool a _
[ ] continue  :: _
[ ] break     :: _
[ ] return a  :: a a

# math
[ ] acos acosh asin asinh atan atan2 atanh cbrt cos cosh erf erfc exp gamma log log10 log2 sin sinh sqrt tan tanh :: float float
[ ] e, pi, inf, nan :: float
[ ] hypot, logn :: float float float
[ ] lgamma, frexp :: float (float, int)
[ ] ldexp :: float int float
