" :h group-name
if exists("b:current_syntax")
  finish
endif

setlocal isk+=!
setlocal isk+=:
setlocal isk+=@
setlocal isk+=-
syn sync minlines=500

" keybord helper
"inoremap {  {<CR>}<Esc>O
"inoremap (  ()<Esc>i
"inoremap [  []<Esc>i
"inoremap "  ""<Esc>i
"inoremap '  ''<Esc>i

"*Comment        o コメント
syn region Comment start=/__comment__/ end="__uncomment__"
syn match  Comment /#.*$/

"*Constant       o 定数
syn region String start='"' end='"' skip='\\"'
syn region String start='`' end='`' skip='\\`'
syn match Number / [0-9]\+\(\.[0-9]\+\)\?/
syn keyword Boolean true false

"*Identifier     o 変数名
syn match Function  /^[a-zA-Z0-9]\+\%(.*=[ \n]\)\@=/
syn match Structure "^[a-zA-Z0-9_.]\+\%([^=]*:\)\@="
"syn region vlBlock start=/:$/ end=/\n[^a-zA-Z]/ contains=vlMember
"syn match vlMember /^[ \t]\+[a-zA-Z_0-9]\+[ \n]/ contained
"hi def link vlMember Identifier

"*Statement      o 命令文
""syn keyword Statement next break return goto
""syn keyword Conditional if else match
""syn keyword Repeat for
""syn keyword Exception catch throw
syn match Operator "|||"
syn match Operator "[+\-\*/|&:]=\="
syn match Operator "[<>]=\="
syn match Operator "=="
syn match Operator "="
syn match Operator "<-"
syn match Operator "->"
syn match Operator "||"
syn match Operator "&&"
syn match Operator " \. "
syn match Operator "|"

syn match Macro /^- [a-zA-Z0-9_ ]*/

"*PreProc        o 一般的なプリプロセッサー命令
""syn match Macro /@[a-zA-Z0-9:_]\+/

"*Type           o int, long, char, その他
""syn keyword StorageClass let var

"*Underlined     o 目立つ文章, HTMLリンク
""syn keyword Define package import export

"hi def link vlRepeat Repeat

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
