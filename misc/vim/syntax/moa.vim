" :h group-name
if exists("b:current_syntax")
  finish
endif

" Comments
syn match  Comment /#.*$/

" Constant
syn region String start=+\z(["']\)+  skip=+\\\%(\z1\|$\)+  end=+\z1+ end=+$+
syn region String start=+\z(```\|"""\|'''\)+  skip=+\\\%(\z1\|$\)+  end=+\z1+ end=+$+
syn region String start=+r\z([`"'/]\)+  skip=+\\\%(\z1\|$\)+  end=+\z1+ end=+$+
syn match Number /\%([A-Za-z_]\)\@<![0-9]\+[xob]\?[A-Fa-f0-9_hmsun]*\(\.[A-Fa-f0-9_]\+\)\?/
syn keyword Boolean true false

" Statement
syn keyword Define      def dec let var struct class enum
syn keyword Boolean     true false
syn keyword Type        _ bool int float string bytes stream fn time duration
syn keyword Type        tuple struct option some none set list dict
syn keyword Conditional iif if else switch
syn keyword Repeat      for while
syn keyword keyword     continue break return throw catch assert
syn keyword Keyword     use module math array nume decimal i8 i16 i32 i64 u8 u16 u32 u64 f32 f64
syn match Operator /[+\-\*/|&]=\=/
syn match Operator /[<>]=\=/
syn match Operator /[=!]=/
syn match Operator /[=!]==/
syn match Operator /[=;,:]/
syn match Operator "<-"
syn match Operator "->"
syn match Operator "||"
syn match Operator "&&"


"*Comment        o コメント
"
"*Constant       o 定数
" String         o 文字列定数: "これは文字列です"
" Character      o 文字定数: 'c', '\n'
" Number         o 数値定数: 234, 0xff
" Boolean        o ブール値の定数: TRUE, false
" Float          o 浮動小数点数の定数: 2.3e10
"
"*Identifier     o 変数名
" Function       o 関数名(クラスメソッドを含む)
"
"*Statement      o 命令文
" Conditional    o if, then, else, endif, switch, その他
" Repeat         o for, do, while, その他
" Label          o case, default, その他
" Operator       o "sizeof", "+", "*", その他
" Keyword        o その他のキーワード
" Exception      o try, catch, throw
"
"*PreProc        o 一般的なプリプロセッサー命令
" Include        o #include プリプロセッサー
" Define         o #define プリプロセッサー
" Macro          o Defineと同値
" PreCondit      o プリプロセッサーの #if, #else, #endif, その他
"
"*Type           o int, long, char, その他
" StorageClass   o static, register, volatile, その他
" Structure      o struct, union, enum, その他
" Typedef        o typedef宣言
"
"*Special        o 特殊なシンボル
" SpecialChar    o 特殊な文字定数
" Tag            o この上で CTRL-] を使うことができる
" Delimiter      o 注意が必要な文字
" SpecialComment o コメント内の特記事項
" Debug          o デバッグ命令
"
"*Underlined     o 目立つ文章, HTMLリンク
"
"*Ignore         o (見た目上)空白, 不可視  hl-Ignore
"
"*Error          o エラーなど、なんらかの誤った構造
"
"*Todo           o 特別な注意が必要なもの; 大抵はTODO FIXME XXXなど
"                  のキーワード
