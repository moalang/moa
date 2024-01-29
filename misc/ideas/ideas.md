# Ideas

TODOs
- type inference
- js: CRWD and graphs
- Go: CRWD and HTTPD
- Swift iOS: memo + TODO + calendar App
- AD for iOS
- Release iOS app
- Release Android app


https://keens.github.io/blog/2021/01/04/future_of_proguramming_languages/
- "ただ、私は何のエラーが上がってくるか型で明示してほしいのと、例外はリスタート可能であってほしいと思っています。"
- "、特に重要そうな部分は証明つけよっかってなるフローです。"
- "継続欲しいですよね。 "

Syntax
s ary =
  [] -> ""
  [a] -> a
  _ -> ary.join("")
s ary = match(
  ary:[] -> ""
  ary:[a] -> a
  _ -> ary.join(""))

add a b =
| int int = a + b
| float float = a + b

Core library
- any
  string : string
  bytes  : bytes
- bool
  # && || : bool bool bool
- int (i8 .. i256, u8 .. u256)
  # + - * **: a a a
  # / : a a float
  # // % : a a some(a)
- float (f8 .. f64)
  # + - * ** / // %: a a a
- char
- string = list(char)
  count : int
- byte
- bytes = list(byte)
- list a
  size : int
  # ++ a : list(a) list(a) list(a)
- io
  reads : io(string)
  write : any io
  puts  : string io
  tcp
  fs
  time
  random
- math
