# Minimal syntax
```
top: (atom | br)+ hint?
atom:
| "(" top+ ")"           # priority, or function call
| " "+                   # space
| [0-9]+ ("." [0-9]+)?   # 1.2
| '"' [^"]* '"'          # "string"
| [A-Za-z_][A-Za-z0-9_]* # id
| op2                    # (+ 1 2)
| ": " atom+             # if ...: ...
| ":" (indent atom)+
br: [ \n]* "\n" [ \n]*
indent: "\n  "
op2:
| "." | "=" | ":=" | "=="
| [+-*/%|&^~<>] "="?
| ("++" | "||" | "&&") "="?
hint: "::" atom+
```

# Example
```
# Primitive
true          # bool
1             # int
1.1           # float
"s"           # string
"""s"""       # string
fn: 1         # function
fn a: a       # function
fn a b: a + b # function

# Value
time "2024-1-1"
duration "1h30m"
regexp "[a-z]+"
bytes "utf8"
stream 1024

# Container
tuple "a" 1
list 1 2
dict "a" 1
set 1 2

# Declear
add n.num:: n n n 
vector2::
  x.int
  y.int
tree t::
| leaf
| node val.t lhs.tree[t] rhs.tree[t]

# Branch
iif a 1 2
iif a 1 b 2 3
iif:
  a: 1
  b: 2
  _: 3
if a     : ...
else if b: ...
else     : ...
switch a:
  list: 0
  list a ...: a
switch tree:
  leaf: 0
  node val lhs rhs: val + rec(lsh) + rec(rhs)

# Loop
for 10: ...
for i 10: ...
for i 1 10: ...
for i 10 -1 -1: ...
while a < b: ...
continue
break

# Exception
catch throw("message" 401): e => e.message
  message.string status.int: $"{message} code({status})"
  e: e.message

# Module
```
