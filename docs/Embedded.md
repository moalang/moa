# global
- [ ] true      :: bool
- [ ] false     :: bool
- [ ] throw a b :: a b
- [ ] catch a   :: (error a) a
- [ ] log       :: ... a a
- [ ] assert    :: bool void
- [ ] iif a     :: bool a ...bool,a a

# num a
- [ ] + - * ** / % | & ^ :: a a a
- [ ] abs, neg :: a
- [ ] string :: string

# int.num
- [ ] char :: string

# float.num
- [ ] floor  :: int
- [ ] ceil   :: int
- [ ] round  :: int

# string
- [ ] ++      :: string string string
- [ ] size    :: int
- [ ] reverse :: string
- [ ] slice   :: int int? string
- [ ] split   :: string int? list[string]
- [ ] index   :: string opt[int]
- [ ] replace :: string string string
- [ ] trim    :: string
- [ ] starts  :: string bool
- [ ] ends    :: string bool
- [ ] has     :: string bool
- [ ] encode  :: string opt[bytes]

# regexp
- [ ] match   :: string bool
- [ ] capture :: string list[string]
- [ ] split   :: string list[string]
- [ ] replace :: string (list[string] string) string

# lambda[a b ...]
...

# list[a]
- [ ] ++ a    :: list[a] list[a]
- [ ] size    :: int
- [ ] get     :: int opt[a]
- [ ] set     :: int a bool
- [ ] at      :: int opt[a]
- [ ] tie     :: int a opt[a]
- [ ] fill    :: a int? int? int
- [ ] push    :: a a
- [ ] map b   :: (a int? b) list[b]
- [ ] fmap b  :: (a int? list[b]) list[b]
- [ ] keep    :: (a int? bool) list[a]
- [ ] all     :: (a int? bool) bool
- [ ] any     :: (a int? bool) bool
- [ ] slice   :: int int? list[a]
- [ ] sort b  :: (a a? b)? list[a]
- [ ] reverse :: list[a]
- [ ] zip b   :: list[b] list[tuple[a b]]
- [ ] fold b  :: b (a b b) b
- [ ] find    :: (a bool) opt[a]
- [ ] index   :: (a bool) opt[int]
- [ ] join    :: string string
- [ ] has     :: a bool
- [ ] min     :: a
- [ ] max     :: a

# dict[k v]
- [ ] size   :: int
- [ ] get    :: k opt[v]
- [ ] set    :: k v bool
- [ ] has    :: k bool
- [ ] keys   :: list[k]
- [ ] values :: list[v]
- [ ] list   :: list[tuple[k v]]

# set[a]
- [ ] size   :: int
- [ ] -      :: set[a] set[a]
- [ ] |      :: set[a] set[a]
- [ ] &      :: set[a] set[a]
- [ ] ^      :: set[a] set[a]
- [ ] has    :: a bool
- [ ] add    :: a bool
- [ ] rid    :: a bool
- [ ] list   :: list[a]

# tuple[a b ...]
- [ ] 0 :: a
- [ ] 1 :: b
- ...

# time
- [ ] year, month, day, hour, min, sec, wday, yday, offset :: int
- [ ] format :: string string
- [ ] utc    :: time
- [ ] string :: string
- [ ] tick   :: int time

# bytes
- [ ] size   :: int
- [ ] at     :: int opt[u8]
- [ ] tie    :: int u8 opt[u8]
- [ ] decode :: string? opt[string]
- [ ] tr     :: string string
- [ ] stream :: stream

# stream
- [ ] size   :: int
- [ ] resize :: int int
- [ ] le     :: bool
- [ ] be     :: bool
- [ ] flush  :: ... opt[int]
- [ ] close  :: opt[bool]
- [ ] closed :: bool
- [ ] read   :: int? opt[bytes]
- [ ] write  :: ... opt[int]
- [ ] i8     :: opt[i8]
- [ ] i16    :: opt[i16]
- [ ] i32    :: opt[i32]
- [ ] i64    :: opt[i64]
- [ ] u8     :: opt[u8]
- [ ] u16    :: opt[u16]
- [ ] u32    :: opt[u32]
- [ ] u64    :: opt[u64]
- [ ] f32    :: opt[f32]
- [ ] f64    :: opt[f64]
- [ ] decode :: string? opt[string]
- [ ] bytes  :: list[u8]


Below is ideas
----------------------------------------------------------------------------

# std
- [ ] argv   :: list[string]
- [ ] env    :: dict[string string]
- [ ] now    :: time
- [ ] random :: int? random
- [ ] db t u :: (t u) u
- [ ] fs     :: string? std.fs
- [ ] shell  :: string ...string opt[stream]
- [ ] http   :: http
- [ ] stdin  :: stream
- [ ] stdout :: stream
- [ ] stderr :: stream

# std.fs
- [ ] path    :: string
- [ ] cd      :: string std.fs
- [ ] open t  :: string? (stream t) opt[t]
- [ ] read    :: opt[stream]
- [ ] reads   :: opt[string]
- [ ] write   :: ... opt[int]
- [ ] append  :: ... opt[int]
- [ ] rm      :: opt[bool]
- [ ] exists  :: opt[bool]
- [ ] glob    :: list[std.fs]

# std.http
- [ ] listen {port=3000} (http.request http.response) opt[_]
- [ ] call string {method="get" headers.list[tuple[string list[string]]] body.stream} http.response
- [ ] request
      method  :: string
      path    :: string
      header  :: string string
      headers :: string list[string]
      query   :: string
      get     :: string string
      gets    :: string list[string]
      post    :: string string
      posts   :: string list[string]
      body    :: stream
- [ ] response
      status  :: int
      header  :: string string
      headers :: string list[string]
      body    :: stream

# std.random
- [ ] int   :: int? int? int
- [ ] float :: float? float? float
- [ ] stream :: int bytes

# std.bcrypt
- [ ] :: string std.bcrypt
- [ ] eq :: string string bool

# std.math
- [ ] acos acosh asin asinh atan atan2 atanh cbrt cos cosh erf erfc exp gamma log log10 log2 sin sinh sqrt tan tanh :: float float
- [ ] e, pi, inf, nan :: float
- [ ] hypot, logn :: float float float
- [ ] lgamma, frexp :: float (float, int)
- [ ] ldexp :: float int float
