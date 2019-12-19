# Build in types
Primitive
- bool
- int
- float
- string
- function
- error
Container
- tuple
- class
- array
- map
- set
- enum
Interface
- void
- number
- try
- eff (rename from seq)
Control flow
- effect
- recursive call
- branch
Binary operators
- effect      : <-
- comparision : == != >= <= > < || &&
- update      : += -= *= /= %=
- array       : ++
- string      : .
- number      : + - * / % **
- available   : @ & &&& ||| //
- reserved    : | , ! ? ^



# Core library
bool:
  guard : string try(void)
int:
  to_s : try(string)
float:
  to_s : try(string)
string:
  join : string string
  split : string string
  to_i : try(int)
  to_f : try(float)
  to_a : string.to_a
function a b c:
  name : string
  bind a : function(b c)
turple a b:
  n0 : a
  n1 : b
array a:
  reduce b : function(a b) b
  map b : b.array
  filter : function(a bool) a.array
  include : a bool
  to_set : a.set
map k v:
  keys : k.array
  values : v.array
set a:
  to_array : a.array
enum:
  keys : string.array
  count : int
number a:
  +, -, *, / : a a a
try a:
  and : a.try a.try
  or : a.try a.try
eff a:
  and : a.try a.try
  or : a.try a.try
  then : function(error eff.a) eff.a
log:
  debug a : a void
  info a : a void
  warn a : a void
  error a : a void



# Reference (F#)
## Expressions
[]        op_Nil
::        op_Cons
+         op_Addition
-         op_Subtraction
*         op_Multiply
/         op_Division
@         op_Append
^         op_Concatenate
%         op_Modulus
&&&       op_BitwiseAnd
|||       op_BitwiseOr
^^^       op_ExclusiveOr
<<<       op_LeftShift
~~~       op_LogicalNot
>>>       op_RightShift
~+        op_UnaryPlus
~-        op_UnaryNegation
=         op_Equality
<=        op_LessThanOrEqual
>=        op_GreaterThanOrEqual
<         op_LessThan
>         op_GreaterThan
?         op_Dynamic
?<-       op_DynamicAssignment
|>        op_PipeRight
<|        op_PipeLeft
!         op_Dereference
>>        op_ComposeRight
<<        op_ComposeLeft
<@ @>     op_Quotation
<@@ @@>   op_QuotationUntyped
+=        op_AdditionAssignment
-=        op_SubtractionAssignment
*=        op_MultiplyAssignment
/=        op_DivisionAssignment
..        op_Range
.. ..     op_RangeStep
>         Greater
<         Less
+         Plus
-         Minus
*         Multiply
/         Divide
=         Equals
~         Twiddle
%         Percent
.         Dot
&         Amp
|         Bar
@         At
^         Hat
!         Bang
?         Qmark
(         LParen
,         Comma
)         RParen
[         LBrack
]         RBrack

## Patterns
定数パターン
1.0, "test", 30, Color.Red

識別子パターン
Some(x)

Failure(msg)
変数パターン
a

as パターン
識別子としてのパターン
(a, b) as tuple1

OR パターン
([h] | [h; _])

AND パターン
(a, b) & (_, "test")

Cons パターン
識別子::リスト-識別子
h :: t

リスト パターン
[ pattern_1;...;pattern_n ]
[ a; b; c ]

配列パターン
[| pattern_1;..;pattern_n |]
[| a; b; c |]

かっこで囲まれたパターン
( a )

タプル パターン
( a, b )

レコード パターン
{ identifier1 = pattern_1;...;identifier_n = pattern_n }
{ Name = name; }

ワイルドカード パターン
_

型の注釈が指定されたパターン
a : int

型テスト パターン
:? 型[as識別子]
:? System.DateTime as dt

null パターン
null

## Special characters
警告                \a
バックスペース      \b
フォーム フィード   \f
改行                \n
キャリッジ リターン \r
タブ                \t
垂直タブ            \v
円記号              \\
引用符              \"
単一                \'
Unicode 文字 \DDD(は10進数字、000-255 の範囲、 \231 = "ç" など) D
Unicode 文字 \xHH(は16進数の数字、00 ~ FF の範囲、 \xE7 = "ç" など) H
