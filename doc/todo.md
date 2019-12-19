# TODO
[x] Design error handling type and sequence syntax
[x] Tidy up documents
[] Make Interpriter
  x REPL
  x bool
  x int
  x float
  x string
  x func
  x closure
  x array
  - dictionary
  x struct
  x enum
  x flow
  x sequence
  x branch
  x binary operators
  x embedded functions
  - generics
[] Compile Moa to C
[] Self booting
[] Make remaining core features
  - coroutine
  - mutable and imutable
  - namespace

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
