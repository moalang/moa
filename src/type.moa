dec log ...a b : ...a b b
dec assert a   : a a _
dec iif a      : ...bool,a a
dec if a b     : a b _
dec else a     : a _
dec switch a b : a ... b
dec throw a b  : string a? b
dec catch a    : a (error a) a
dec return a   : a a
dec for a      : _id int int? int? a _
dec while a    : bool a _
dec continue   : _
dec break      : _

enum bool:
  true
  false
  !  : bool bool
  || : bool bool bool
  && : bool bool bool

interface _cmp a:
  == a a bool
  != a a bool
  <  a a bool
  <= a a bool
  >  a a bool
  >= a a bool

interface num a:
  _cmp
  +   a a a
  -   a a a
  *   a a a
  /   a a a
  %   a a a
  **  a a a
  abs : a
  neg : a

interface _num a:
  .new b._num : b a

interface _int a:
  _num
  ~    a a
  &    a a a
  |    a a a
  ^    a a a
  <<   a a a
  >>   a a a
  char : string
extern int: _int
extern  i8: _int
extern i16: _int
extern i32: _int
extern i64: _int
extern  u8: _int
extern u16: _int
extern u32: _int
extern u64: _int

interface _float a:
  .inf  a
  .nan  a
  _num
  floor : int
  ceil  : int
  round : int
extern float: _float
extern   f32: _float
extern   f64: _float

extern string:
  _cmp
  .new    : _ string
  size    int
  concat  : string string
  reverse : string
  slice   : int? int? int? string
  split   : string int? list[string]
  index   : string int!
  replace : string string string
  trim    : string
  starts  : string bool
  ends    : string bool
  has     : string bool
  repeat  : int string
  int     : int!
  float   : int!

extern fn ...a:
  _cmp

class error a:
  _cmp
  message   string
  backtrace string
  detail    a

extern tuple ...a:
  _cmp

extern list a:
  _cmp
  size    int
  concat  : list[a] list[a]
  slice   : int? int? int? list[a]
  get     : int a!
  set     : int a a!
  push    : a a
  map b   : (a int? b) list[b]
  fmap b  : (a int? list[b]) list[b]
  keep    : (a int? bool) list[a]
  all     : (a int? bool) bool
  any     : (a int? bool) bool
  sort b  : (a a? b)? list[a]
  reverse : list[a]
  zip b   : list[b] list[tuple[a b]]
  fold b  : b (a b b) b
  find    : (a bool) a!
  index   : (a bool) int!
  join    : string string
  has     : a bool
  min     : a
  max     : a
  repeat  : int list[a]

extern set a:
  _cmp
  size int
  -    : set[a] set[a]
  |    : set[a] set[a]
  &    : set[a] set[a]
  ^    : set[a] set[a]
  has  : a bool
  add  : a bool
  rid  : a bool
  list : list[a]

extern dict k v:
  _cmp
  size   int
  get    : k v!
  set    : k v v
  has    : k bool
  keys   : list[k]
  values : list[v]
  items  : list[tuple[k v]]
  concat : dict[k v] dict[k v]
