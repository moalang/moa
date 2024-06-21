# global
- [ ] true      :: bool
- [ ] false     :: bool
- [ ] throw a b :: a b
- [ ] catch a   :: (error a) a
- [ ] log       :: ... a a
- [ ] assert    :: bool void

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
- [ ] encode  :: string opt[buffer]
- [ ] copy    :: string

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
- [ ] copy    :: list[a]

# dict[k v]
- [ ] size   :: int
- [ ] get    :: k opt[v]
- [ ] set    :: k v bool
- [ ] has    :: k bool
- [ ] keys   :: list[k]
- [ ] values :: list[v]
- [ ] list   :: list[tuple[k v]]
- [ ] copy   :: dict[k v]

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
- [ ] copy   :: set[a]

# tuple[a b ...]
- [ ] 0 :: a
- [ ] 1 :: b
- ...
- [ ] copy :: tuple[a b]

# time
- [ ] year, month, day, hour, min, sec, wday, yday, offset :: int
- [ ] format :: string string
- [ ] utc    :: time
- [ ] string :: string
- [ ] tick   :: int time

# buffer
- [ ] size   :: int
- [ ] resize :: int int
- [ ] offset :: int
- [ ] seek   :: int int
- [ ] at     :: int opt[u8]
- [ ] tie    :: int u8 opt[u8]
- [ ] read   :: int? opt[list[u8]]
- [ ] write  :: ... opt[int]
- [ ] flush  :: ... opt[int]
- [ ] close  :: opt[bool]
- [ ] closed :: bool
- [ ] le     :: opt[_]
- [ ] be     :: opt[_]
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
- [ ] tr     :: string string
- [ ] bytes  :: list[u8]


Below is ideas
----------------------------------------------------------------------------

# std
- [ ] argv   :: list[string]
- [ ] env    :: string string
- [ ] now    :: time
- [ ] rand   :: rand
- [ ] db t u :: (t u) u
- [ ] fs     :: string? std.fs
- [ ] shell  :: string ...string opt[buffer]
- [ ] http   :: http
- [ ] stdin  :: buffer
- [ ] stdout :: buffer
- [ ] stderr :: buffer

# std.fs
- [ ] path    :: string
- [ ] cd      :: string std.fs
- [ ] open t  :: string? (buffer t) opt[a]
- [ ] read    :: opt[buffer]
- [ ] reads   :: opt[string]
- [ ] write   :: ... opt[int]
- [ ] append  :: ... opt[int]
- [ ] rm      :: opt[bool]
- [ ] exists  :: opt[bool]
- [ ] glob    :: list[std.fs]

# std.http
- [ ] listen {port=3000} (http.request http.response) opt[_]
- [ ] call string {method="get" headers.list[tuple[string list[string]]] body.buffer} http.response
- [ ] request
      method  :: string
      path    :: string
      has     :: string bool
      header  :: string string
      headers :: string list[string]
      get     :: string string
      gets    :: string list[string]
      post    :: string string
      posts   :: string list[string]
      body    :: buffer
- [ ] response
      status  :: int
      has     :: string bool
      header  :: string string
      headers :: string list[string]
      body    :: buffer

# std.rand
- [ ] int   :: int? int? int
- [ ] float :: float? float? float
- [ ] buffer :: int buffer

# std.bcrypt
- [ ] :: string std.bcrypt
- [ ] eq :: string string bool

# std.math
- [ ] acos acosh asin asinh atan atan2 atanh cbrt cos cosh erf erfc exp gamma log log10 log2 sin sinh sqrt tan tanh :: float float
- [ ] e, pi, inf, nan :: float
- [ ] hypot, logn :: float float float
- [ ] lgamma, frexp :: float (float, int)
- [ ] ldexp :: float int float
