# global
- [x] _            :: write only variable
- [x] void         :: void
- [x] true, false  :: bool
- [x] some a, none :: option[a]
- [x] throw a b    :: a b
- [x] catch a      :: (error a) a
- [x] iif a        :: ...[bool a] a
- [x] case a b     :: a ...[a b] b
- [x] if, else
- [x] while, continue, break

# option a
- [x] and b :: option[a] (a b) option[b]
- [x] or    :: option[a] a a
- [x] bool  :: bool

# num a
- [x] + - * ** / % | & ^ :: a a a
- [x] abs, neg :: a

# int.num
- [x] char   :: string

# float.num
- [x] floor  :: int
- [x] ceil   :: int
- [x] round  :: int

# string
- [x] ++      :: string string string
- [x] size    :: int
- [x] reverse :: string
- [x] slice   :: int int? string
- [x] split   :: string int? list[string]
- [x] index   :: string option[int]
- [x] replace :: string string string
- [x] trim    :: string
- [x] starts  :: string bool
- [x] ends    :: string bool
- [x] has     :: string bool

# regexp
- [x] match   :: string bool
- [x] capture :: string list[string]
- [x] split   :: string list[string]
- [x] replace :: string (list[string] string) string

# lambda[a b ...]
- [x]

# list[a]
- [x] ++ a    :: list[a] list[a]
- [x] size    :: int
- [x] get     :: int option[a]
- [x] set     :: int a bool
- [x] at      :: int a @error
- [x] tie     :: int a a @error
- [x] push    :: a a
- [x] map b   :: (a b) list[b]
- [x] fmap b  :: (a list[b]) list[b]
- [x] keep    :: (a bool) list[a]
- [x] all     :: (a bool) bool
- [x] any     :: (a bool) bool
- [x] slice   :: int int? list[a]
- [x] sort    :: (a a bool)? list[a]
- [x] reverse :: list[a]
- [x] zip b   :: list[b] list[tuple[a b]]
- [x] fold b  :: b (a b b) b
- [x] find    :: (a bool) option[a]
- [x] index   :: (a bool) option[int]
- [x] join    :: string string
- [x] has     :: a bool
- [x] min     :: a
- [x] max     :: a

# dict[k v]
- [x] size   :: int
- [x] get    :: k option[v]
- [x] set    :: k v bool
- [x] has    :: k bool
- [x] keys   :: list[k]
- [x] values :: list[v]
- [x] list   :: list[tuple[k v]]

# set[a]
- [x] size   :: int
- [x] -      :: set[a] set[a]
- [x] |      :: set[a] set[a]
- [x] &      :: set[a] set[a]
- [x] ^      :: set[a] set[a]
- [x] has    :: a bool
- [x] add    :: a bool
- [x] rid    :: a bool
- [x] list   :: list[a]

# tuple[a b ...]
- [x] 0 :: a
- [x] 1 :: b
- ...

# struct
[x]

# time
- [x] year, month, day, hour, min, sec, wday, yday, offset :: int
- [x] format :: string string
- [x] utc    :: time
- [x] string :: string
- [x] tick   :: int time

# shell
- [x] result :: string @error

# ---( standard module )-------------------------------

# log
- [x] :: a ... a

# assert
- [x] :: a a void

# io
- [x] argv   :: list[string]
- [-] env    :: string option[string]
- [-] now    :: time
- [x] fs     :: fs
- [x] rand   :: float
- [x] shell  :: string ..string shell
- [x] print  :: ...any void
- [-] stdin  :: stream
- [ ] stdout :: stream
- [ ] stderr :: stream

# fs
- [x] open   :: string stream @error

# steram
- [ ] read             :: int? buffer @error
- [ ] write a          :: a int @error
- [ ] offset           :: int
- [ ] seek             :: int int @error
- [ ] flush            :: any? int @error
- [ ] close            :: @error
- [ ] closed           :: bool
- [ ] peek             :: Buffer
- [ ] le               :: stream
- [ ] be               :: stream
- [ ] i8,i16,i32,i64   :: int @error
- [ ] u8,u16,u32,u64   :: int @error
- [ ] f32,f64          :: float @error
- [-] utf8,utf16,utf32 :: string @error

# buffer
- [ ] size             :: int
- [ ] i8,i16,i32,i64   :: int option[int]
- [ ] u8,u16,u32,u64   :: int option[int]
- [ ] f32,f64          :: int option[float]
- [ ] utf8,utf16,utf32 :: int int? option[string]
- [ ] slice            :: int int? buffer

# ---( pending )---------------------------------------
# io
- [ ] path string:
- [ ]   path   :: string
- [ ]   join   :: string path
- [ ]   glob   :: string list[path]
- [ ]   read   :: option[bytes]
- [ ]   write  :: bytes _ @error
- [ ]   append :: bytes _ @error
- [ ]   unlink :: @error
- [ ] database t u :: (t u) u
- [ ] randint :: int int int
- [ ] randbytes :: int bytes
- [ ] http:
- [ ]   listen (http.request http.response) _
- [ ]   call string {method.string="get" headers.list[tuple[string list[string]]]=[] body.bytes=[]} http.response
- [ ]   get url option.. = call url {method="get" option..}
- [ ]   post url option.. = call url {method="post" option..}
- [ ]   request:
        method  :: string
        path    :: string
        header  :: string string
        headers :: string list[string]
        get     :: string string
        gets    :: string list[string]
        post    :: string string
        posts   :: string list[string]
        body    :: bytes
- [ ]   response:
        status  :: int
        headers :: list[tuple[string string]]
        body    :: bytes

# bcrypt
- [ ] encrypt :: string
- [ ] eq      :: string string bool


# interface
[ ] eq = interface a:
  eq: a a bool
  ne l r = !eq l r
  time.eq.eq l r = l.int == r.int

# statement
- [ ] if a      :: bool a _
- [ ] else a    :: a _
- [ ] for a     :: .int int int? int? a _
- [ ] each a b  :: .a seq[a] b _
- [ ] while a   :: bool a _
- [ ] continue  :: _
- [ ] break     :: _
- [ ] return a  :: a a

# math
- [ ] acos acosh asin asinh atan atan2 atanh cbrt cos cosh erf erfc exp gamma log log10 log2 sin sinh sqrt tan tanh :: float float
- [ ] e, pi, inf, nan :: float
- [ ] hypot, logn :: float float float
- [ ] lgamma, frexp :: float (float, int)
- [ ] ldexp :: float int float
