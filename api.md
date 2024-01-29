# reserved
[ ] _            :: write only variable?
[x] void         :: void
[x] true, false  :: bool
[ ] none a       :: opt[a]
[ ] throw a b    :: a b
[ ] catch a      :: (error a) a

# opt a
[ ] and b    :: opt[a] (a b) opt[b]
[ ] or       :: opt[a] a a
[ ] default  :: a

# num a
[x] + - * ** / % | & ^ :: a a a
[x] abs, neg :: a

# int.num
[x] char   :: string

# float.num
[x] floor  :: int
[x] ceil   :: int
[x] round  :: int

# string
[x] ++      :: string string string
[x] size    :: int
[x] reverse :: string
[x] slice   :: int int? string
[x] split   :: string int? list[string]
[ ] index   :: string opt[int]
[x] replace :: string string string

# regexp
[ ] match   :: string opt[list[string]]
[x] split   :: string list[string]
[x] replace :: string (list[string] string) string

# lambda[a b ...]
[x]

# list[a]
[x] ++ a    :: list[a] list[a]
[x] size    :: int
[ ] get     :: int opt[a]
[ ] set     :: int a bool
[-] []      :: int a @error
[-] []=     :: int a a @error
[-] map b   :: (a b) list[b]
[-] fmap b  :: (a list[b]) list[b]
[-] keep    :: (a bool) list[a]
[-] all     :: (a bool) bool
[-] any     :: (a bool) bool
[x] slice   :: int int? list[a]
[ ] sort b  :: (a b)? list[a]
[ ] count b :: (a b)? dict[b int]
[ ] group b :: (a b)? dict[b list[a]]
[x] reverse :: list[a]
[ ] zip b   :: list[b] list[tuple[a b]]
[ ] fold b  :: (a b b) b? b
[ ] find    :: (a bool) opt[a]
[-] join    :: string string
[-] has     :: a bool
[-] min     :: a
[-] max     :: a

# dict[k v]
[x] size   :: int
[ ] get    :: k opt[v]
[-] set    :: k v bool
[-] has    :: k bool
[-] keys   :: list[k]
[-] values :: list[v]
[-] list   :: list[tuple[k v]]

# set[a]
[x] size   :: int
[x] -      :: set[a] set[a]
[x] |      :: set[a] set[a]
[x] &      :: set[a] set[a]
[x] ^      :: set[a] set[a]
[x] has    :: a bool
[x] add    :: a bool
[x] rid    :: a bool
[x] list   :: list[a]

# tuple[a b ...]
[x] 0 :: a
[x] 1 :: b
...

# time
[-] year, month, day, hour, min, sec, wday, yday, offset, int :: int
[-] format :: string string
[-] zone   :: string
[-] utc    :: time
[-] tick   :: int time

# log
[x] :: a ... a

# assert
[x] :: a a void

# io
[-] argv   :: list[string]
[-] now    :: time
[-] rand   :: float
[ ] stdin  :: stream
[ ] stdout :: stream
[ ] stderr :: stream

# ---( standard module )-------------------------------

# steram
[ ] le               :: stream
[ ] be               :: stream
[ ] i8,i16,i32,i64   :: int @error
[ ] u8,u16,u32,u64   :: int @error
[ ] f32,f64          :: int @error
[ ] slice            :: int int? buffer @error
[ ] write a          :: a int @error
[ ] utf8,utf16,utf32 :: string @error

# buffer
[ ] size             :: int
[ ] utf8,utf16,utf32 :: opt[string]
[ ] i8,i16,i32,i64   :: int opt[int]
[ ] u8,u16,u32,u64   :: int opt[int]
[ ] f32,f64          :: int opt[float]
[ ] slice            :: int int? buffer

# ---( pending )---------------------------------------
# io
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

# bcrypt
[ ] encrypt :: string
[ ] eq      :: string string bool


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
