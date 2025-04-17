# Syntax
```
top: line*
line: exp+ comment? "\n"
exp: op1? atom (prop | call | index)* (op2 exp)?
atom:
| "(" exp* ")"
| "{" line+ "}"
| "-"? [0-9]+ ("." [0-9]+)? ("e" [0-9]+)? # -1.2
| "-"? "0x" [0-9a-fA-F_]+                 # 0xff -> 255
| '"' [^"]* '"'                           # "string"
| '"""' [^"]* '"""'                       # """a="b"""" -> "a=\"b\""
| '`' [^"]* '`'                           # `regexp`
| id
prop: "." (id | [0-9]+)                   # property access
call: "(" exp* ")"                        # call function
call: "[" exp "]"                         # index access
op1: [!-~] | "..."
op2: [+-*/%<>|&^=!]+
id: [A-Za-z_][A-Za-z0-9_]* [!?]?
comment: "#" [^\n]*
```
