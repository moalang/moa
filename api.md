# reserved
[ ] _           :: tuple[]
[ ] true, false :: bool
[ ] int         :: string opt[int]
[ ] float       :: string opt[float]
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

# struct
[ ] string :: string
[ ] format :: string string
[ ] bool   :: bool
[ ] hash   :: int
[ ] bytes  :: bytes
[ ] json   :: string

# opt a
[ ] and b   :: opt[a] (a b) opt[b]
[ ] or      :: opt[a] a a
[ ] catch   :: (string a) a

# num a
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
[ ] ++         :: string string string
[ ] size       :: int
[ ] pos        :: int
[ ] get        :: int opt[string]
[ ] slice      :: int int? string
[ ] split      :: string int? list[string]
[ ] index      :: string int
[ ] replace    :: string string int? string
[ ] reverse    :: string
[ ] utf8       :: list[int]
[ ] find       :: string list[string]
[ ] starts     :: string bool
[ ] ends       :: string bool
[ ] rsplit     :: string int? list[string]
[ ] rreplace   :: string (list[string] string) int? string

# bytes
[ ] ++      :: bytes bytes bytes
[ ] get     :: int opt[int]
[ ] set     :: int int bool
[ ] []      :: int int @error
[ ] []=     :: int int int @error
[ ] size    :: int
[ ] slice   :: int int? bytes
[ ] string  :: string
[ ] base64  :: string

# lambda[a b ...]

# list[a]
[ ] ++ a    :: list[a] list[a]
[ ] size    :: int
[ ] get     :: int opt[a]
[ ] set     :: int a bool
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
[ ] find    :: (a bool) opt[a]
[ ] fold b  :: (a b b) b
[ ] has     :: a bool
[ ] sum     :: a
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
[ ] year, month, day, hour, min, sec, wday, yday, timestamp :: int
[ ] zone           :: string
[ ] +              :: time int time
[ ] -              :: time time int

# ---( standard module )-------------------------------

# io
[ ] argv :: list[string]
[ ] put  :: string _ @error
[ ] puts :: string _ @error
[ ] path:
[ ]   glob     :: string list[path]
[ ]   read     :: string? bytes @error
[ ]   write    :: bytes int @error
[ ]   delete _ :: @error

# ---( pending )---------------------------------------
# io
[ ] path:
[ ]   join     :: string path
[ ]   read     :: bytes @error
[ ]   write    :: bytes int @error
[ ]   delete _ :: @error
[ ] now  :: time
[ ] randint :: int int int
[ ] randbytes :: int bytes
[ ] database t u :: t (u) u
[ ] http:
[ ]   listen (http.request http.response) _
[ ]   call string {method.string="get" headers.list[tuple[string list[string]]]=[] body.bytes=[]}? http.response
[ ]   get url option... = call url {method="get" ...option}
[ ]   post url option... = call url {method="post" ...option}
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
interface num a:
  + - * ** / / % :: a a a
  abs :: a
  neg :: a

implement int num:
  + a b: ...

# statement
[ ] if a        :: bool a _
[ ] else a      :: a _
[ ] for a       :: .int int int? int? a _
[ ] each a b    :: .a seq[a] b _
[ ] while a     :: bool a _
[ ] continue    :: _
[ ] break       :: _
[ ] return a    :: a a

# math
[ ] acos acosh asin asinh atan atan2 atanh cbrt cos cosh erf erfc exp gamma log log10 log2 sin sinh sqrt tan tanh :: float float
[ ] e, pi, inf, nan :: float
[ ] hypot, logn :: float float float
[ ] lgamma, frexp :: float (float, int)
[ ] ldexp :: float int float

# log
[ ] debug, info, warn, error a :: a any* a
