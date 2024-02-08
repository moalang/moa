# global
- [x] _            :: write only variable
- [x] void         :: void
- [x] true, false  :: bool
- [x] some a, none :: option[a]
- [x] throw a b    :: a b
- [x] catch a      :: fn[error a] a
- [x] iif a        :: ...[bool a] a
- [x] case a b     :: a ...[a b] b
- [x] if, else
- [x] while, continue, break

# option a
- [x] and b :: option[a] fn[a b] option[b]
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
- [ ] encode  :: string string? bytes @error

# regexp
- [x] match   :: string bool
- [x] capture :: string list[string]
- [x] split   :: string list[string]
- [x] replace :: string fn[list[string] string] string

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
- [x] map b   :: fn[a b] list[b]
- [x] fmap b  :: fn[a list[b]] list[b]
- [x] keep    :: fn[a bool] list[a]
- [x] all     :: fn[a bool] bool
- [x] any     :: fn[a bool] bool
- [x] slice   :: int int? list[a]
- [x] sort    :: fn[a a bool]? list[a]
- [x] reverse :: list[a]
- [x] zip b   :: list[b] list[tuple[a b]]
- [x] fold b  :: b fn[a b b] b
- [x] find    :: fn[a bool] option[a]
- [x] index   :: fn[a bool] option[int]
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
- [x] argv       :: list[string]
- [-] env        :: string option[string]
- [-] now        :: time
- [x] fs         :: fs
- [-] rand       :: rand
- [x] shell      :: string ...string shell
- [x] print      :: ... void
- [-] stdin      :: stream
- [ ] stdout     :: stream
- [ ] stderr     :: stream

# rand
- [ ] int        :: int? int? int
- [ ] float      :: float? float? float
- [ ] bytes      :: int bytes

# fs
- [-] open t  :: string string? fn[stream t] a @error
- [-] read    :: string bytes @error
- [-] reads   :: string string @error
- [-] write   :: string ...serial int @error
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
- [x] utf8             :: string @error
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
- [ ] int.serial
- [ ] float.serial
- [ ] string.serial
- [ ] time.serial
- [ ] tuple.serial
- [ ] struct.serial
- [ ] list.serial
- [ ] set.serial
- [ ] dict.serial
- [ ] stream.serial
- [ ] bytes.serial
- [ ] option.serial
- [ ] regexp.serial

# example: json
enum json:
  jstring string
  jlist list[json]
  jdict dict[string json]
impl json serial:
  serialize w:
    jstring s: w "\""; s.each(c => w(jquote(c))); w "\""
    jlist   l: w "["; l.each(w); w "]"
    jdict   d: w "{"; l.each(fn((k v) w k; w v)) w "}"

# ---( pending )---------------------------------------
# io
- [ ] database t u :: (t u) u
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
