# global
- [ ] true      :: bool
- [ ] false     :: bool
- [ ] throw a b :: a b
- [ ] catch a   :: (error a) a
- [ ] pr        :: a ... a
- [ ] assert    :: a a void

# num a
- [ ] + - * ** / % | & ^ :: a a a
- [ ] abs, neg :: a

# int.num
- [ ] char   :: string

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
- [ ] encode  :: string string? bytes @error

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
- [ ] at      :: int a @error
- [ ] tie     :: int a a @error
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

# time
- [ ] year, month, day, hour, min, sec, wday, yday, offset :: int
- [ ] format :: string string
- [ ] utc    :: time
- [ ] string :: string
- [ ] tick   :: int time

# bytes
- [ ] size   :: int
- [ ] at     :: int a @error
- [ ] tie    :: int a a @error
- [ ] slice  :: int int? bytes
- [ ] fill a :: a int? int? bytes @error
- [ ] tr     :: string string
- [ ] io     :: io


# ---( standard module )-------------------------------

# std
- [ ] argv       :: list[string]
- [ ] db t u     :: (t u) u
- [-] env        :: string string
- [-] now        :: time
- [ ] fs         :: string string std.fs
- [-] rand       :: rand
- [ ] shell      :: string ...string bytes @error
- [-] http       :: http
- [-] stdin      :: io
- [-] stdout     :: io
- [-] stderr     :: io

# std.fs
- [ ] path    :: string
- [ ] open t  :: string? (io t) a @error
- [ ] read    :: bytes @error
- [ ] reads   :: string @error
- [ ] write   :: ... int @error
- [ ] append  :: ... int @error
- [ ] rm      :: bool @error
- [ ] exists  :: bool @error
- [ ] glob    :: list[std.fs]

# std.http
- [ ] listen (http.request http.response) _ @error
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
      headers :: list[tuple[string string]]
      body    :: bytes

# std.rand
- [-] int        :: int? int? int
- [-] float      :: float? float? float
- [-] bytes      :: int bytes

# std.io
- [ ] offset           :: int
- [ ] seek             :: int io @error
- [ ] read             :: int? bytes @error
- [ ] write            :: ... int @error
- [ ] deserialize a    :: a @error
- [ ] flush            :: ... int @error
- [ ] close            :: bool @error
- [ ] closed           :: bool
- [ ] peek             :: bytes
- [ ] le               :: io
- [ ] be               :: io
- [ ] i8,i16,i32,i64   :: i8,i16,i32,i64 @error
- [ ] u8,u16,u32,u64   :: u8,u16,u32,u64 @error
- [ ] f32,f64          :: f32,f64        @error
- [ ] utf8             :: string @error
- [ ] decode           :: string string @error

# std.bcrypt
- [ ] encrypt :: string
- [ ] eq      :: string string bool

# std.math
- [ ] acos acosh asin asinh atan atan2 atanh cbrt cos cosh erf erfc exp gamma log log10 log2 sin sinh sqrt tan tanh :: float float
- [ ] e, pi, inf, nan :: float
- [ ] hypot, logn :: float float float
- [ ] lgamma, frexp :: float (float, int)
- [ ] ldexp :: float int float
