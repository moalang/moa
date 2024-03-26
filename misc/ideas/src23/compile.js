/*
# global
- [ ] true         :: bool
- [ ] false        :: bool
- [ ] some a       :: option[a]
- [ ] none         :: option[a]
- [ ] def ... a    :: fn[... a]
- [ ] throw a b    :: a b
- [ ] catch a      :: fn[error a] a
- [ ] if a         :: ...[bool a] a
- [ ] case a b     :: a ...[a b] b
- [ ] fn ... a     :: fn[... a]

# option a
- [ ] and b :: option[a] fn[a b] option[b]
- [ ] or    :: option[a] a a
- [ ] bool  :: bool

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
- [ ] index   :: string option[int]
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
- [ ] replace :: string fn[list[string] string] string

# list[a]
- [ ] ++ a    :: list[a] list[a]
- [ ] size    :: int
- [ ] get     :: int option[a]
- [ ] set     :: int a bool
- [ ] at      :: int a @error
- [ ] tie     :: int a a @error
- [ ] push    :: a a
- [ ] map b   :: fn[a b] list[b]
- [ ] mapi b  :: fn[a int b] list[b]
- [ ] fmap b  :: fn[a list[b]] list[b]
- [ ] keep    :: fn[a bool] list[a]
- [ ] all     :: fn[a bool] bool
- [ ] any     :: fn[a bool] bool
- [ ] slice   :: int int? list[a]
- [ ] sort    :: fn[a a bool]? list[a]
- [ ] reverse :: list[a]
- [ ] zip b   :: list[b] list[tuple[a b]]
- [ ] fold b  :: b fn[a b b] b
- [ ] find    :: fn[a bool] option[a]
- [ ] index   :: fn[a bool] option[int]
- [ ] join    :: string string
- [ ] has     :: a bool
- [ ] min     :: a
- [ ] max     :: a

# dict[k v]
- [ ] size   :: int
- [ ] get    :: k option[v]
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
[ ]

# time
- [ ] year, month, day, hour, min, sec, wday, yday, offset :: int
- [ ] format :: string string
- [ ] utc    :: time
- [ ] string :: string
- [ ] tick   :: int time

# ---( standard module )-------------------------------
# io
- [ ] argv       :: list[string]
- [?] env        :: string option[string]
- [?] now        :: time
- [ ] sh         :: string ...string string @error
- [ ] puts       :: ... void
- [ ] print      :: ... void
- [?] stdin      :: stream
- [?] stdout     :: stream
- [?] stderr     :: stream

# io.rand
- [ ] int        :: int? int? int
- [ ] float      :: float? float? float
- [ ] bytes      :: int bytes

# io.fs
- [-] open t  :: string string? fn[stream t] a @error
- [-] read    :: string bytes @error
- [x] reads   :: string string @error
- [x] write   :: string ...serial int @error
- [x] rm      :: string bool @error
- [-] append  :: string ...serial int @error

# stream
- [ ] offset           :: int
- [ ] seek             :: int stream @error
- [ ] read             :: int? bytes @error
- [ ] write            :: ...serial int @error
- [ ] flush            :: ...serial int @error
- [ ] close            :: bool @error
- [ ] closed           :: bool
- [ ] peek             :: bytes
- [ ] le               :: stream
- [ ] be               :: stream
- [ ] i8,i16,i32,i64   :: i8,i16,i32,i64 @error
- [ ] u8,u16,u32,u64   :: u8,u16,u32,u64 @error
- [ ] f32,f64          :: f32,f64        @error
- [ ] utf8             :: string @error
- [ ] decode           :: string string @error

# bytes
- [ ] size             :: int
- [ ] slice            :: int int? bytes
- [ ] i8,i16,i32,i64   :: option[i8,i16,i32,i64] @error
- [ ] u8,u16,u32,u64   :: option[u8,u16,u32,u64] @error
- [ ] f32,f64          :: option[f32,f64       ] @error
- [ ] tr               :: string string
- [ ] utf8             :: option[string]
- [ ] decode           :: string option[string]
- [ ] deserialize t    :: option[t]
- [ ] stream           :: stream

# serial a
- [ ] serialize :: fn[serial void] void @error

# io.db
- [ ] _ t u            :: fn[t u] u

# io.http
- [ ] listen (http.request http.response) @noret
- [ ] call http.request http.response
- [ ] request:
        version :: string
        method  :: string
        path    :: string
        headers :: list[string,string]
        body    :: bytes
      .methods
        header  :: string string
        get     :: string string
        gets    :: string list[string]
        post    :: string string
        posts   :: string list[string]
- [ ]   response:
        version :: string
        status  :: int
        headers :: list[string,string]
        body    :: bytes
      .methods
        header  :: string string

# bcrypt
- [ ] encrypt :: string
- [ ] eq      :: string string bool

# math
- [ ] acos acosh asin asinh atan atan2 atanh cbrt cos cosh erf erfc exp gamma log log10 log2 sin sinh sqrt tan tanh :: float float
- [ ] e, pi, inf, nan :: float
- [ ] hypot, logn :: float float float
- [ ] lgamma, frexp :: float (float, int)
- [ ] ldexp :: float int float
*/
