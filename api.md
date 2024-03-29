# global
- [ ] true      :: bool
- [ ] false     :: bool
- [ ] throw a b :: a b
- [ ] catch a   :: (error a) a
- [ ] pr        :: a ... a
- [ ] assert    :: bool void

# num a
- [ ] + - * ** / % | & ^ :: a a a
- [ ] abs, neg :: a

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
- [ ] encode  :: string string? opt[bytes]

# regexp
- [ ] match   :: string bool
- [ ] capture :: string list[string]
- [ ] split   :: string list[string]
- [ ] replace :: string (list[string] string) string

# lambda[a b ...]
- [ ]

# list[a]
- [ ] ++ a    :: list[a] list[a]
- [ ] size    :: int
- [ ] get     :: int opt[a]
- [ ] set     :: int a bool
- [ ] at      :: int opt[a]
- [ ] tie     :: int a opt[a]
- [ ] push    :: a a
- [ ] map b   :: (a b) list[b]
- [ ] mapi b  :: (a int b) list[b]
- [ ] fmap b  :: (a list[b]) list[b]
- [ ] keep    :: (a bool) list[a]
- [ ] all     :: (a bool) bool
- [ ] any     :: (a bool) bool
- [ ] slice   :: int int? list[a]
- [ ] sort    :: (a a bool)? list[a]
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

# struct
- [ ]

# time
- [ ] year, month, day, hour, min, sec, wday, yday, offset :: int
- [ ] format :: string string
- [ ] utc    :: time
- [ ] string :: string
- [ ] tick   :: int time

# bytes
- [ ] size   :: int
- [ ] at     :: int opt[a]
- [ ] tie    :: int a opt[a]
- [ ] slice  :: int int? bytes
- [ ] fill a :: a int? int? opt[bytes]
- [ ] tr     :: string string
- [ ] io     :: io

# std
- [ ] argv       :: list[string]
- [ ] env        :: string string
- [ ] now        :: time
- [ ] rand       :: rand
- [ ] db t u     :: (t u) u
- [ ] fs         :: string? std.fs
- [ ] shell      :: string ...string opt[bytes]
- [ ] http       :: http
- [ ] stdin      :: io
- [ ] stdout     :: io
- [ ] stderr     :: io

# std.fs
- [ ] path    :: string
- [ ] cd      :: string std.fs
- [ ] open t  :: string? (io t) opt[a]
- [ ] read    :: opt[bytes]
- [ ] reads   :: opt[string]
- [ ] write   :: ... opt[int]
- [ ] append  :: ... opt[int]
- [ ] rm      :: opt[bool]
- [ ] exists  :: opt[bool]
- [ ] glob    :: list[std.fs]

# std.http
- [ ] listen (http.request http.response) opt[_]
- [ ] call string {method.string="get" headers.list[tuple[string list[string]]]=[] body.bytes=[]} http.response
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
      body    :: bytes
- [ ] response
      status  :: int
      has     :: string bool
      header  :: string string
      headers :: string list[string]
      body    :: bytes

# std.rand
- [ ] int   :: int? int? int
- [ ] float :: float? float? float
- [ ] bytes :: int bytes

# std.io
- [ ] offset :: int
- [ ] seek   :: int opt[io]
- [ ] read   :: int? opt[bytes]
- [ ] write  :: ... opt[int]
- [ ] flush  :: ... opt[int]
- [ ] close  :: opt[bool]
- [ ] closed :: bool
- [ ] peek   :: bytes
- [ ] le     :: io
- [ ] be     :: io
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
- [ ] utf8   :: opt[string]
- [ ] utf16  :: opt[string]
- [ ] decode :: string opt[string]

# std.bcrypt
- [ ] :: string std.bcrypt
- [ ] eq :: string string bool

# std.math
- [ ] acos acosh asin asinh atan atan2 atanh cbrt cos cosh erf erfc exp gamma log log10 log2 sin sinh sqrt tan tanh :: float float
- [ ] e, pi, inf, nan :: float
- [ ] hypot, logn :: float float float
- [ ] lgamma, frexp :: float (float, int)
- [ ] ldexp :: float int float
