# Backlog
- [x] Define syntax
- [x] Define core
- [x] Define test
- [ ] Compile to JavaScript
- [ ] Compile to Go
- [ ] Define block for "while", "for", "each", "continue" and "break"
- [ ] Define UI framework for compiled JavaScript
- [ ] Define Backend framework for compiled Go
- [ ] Make Web IDE

# Ideas
- [ ] Define method for user defined class
- [ ] Define typed argument
- [ ] Define named block
- [ ] Define standard libraries
- [ ] Define checked and unchecked for operator of sized integers

# Allowed
- variadic arguments: for "list"
- exception: for error handling
- option: result type
- overload: for property and method type inference, functions

# Rejected
- number interface: "**" is not good match for "vector2"
- ";": ambiguous in class and enum, parser would be harder
- optional argument: low frequency
- default argument: low frequency
- static member of type: low frequency
- covinient error handling: use "option" for most case
- macro: hard to understand the result

# Pending
- ordered dict: convinient, but slower
- property: intuitiveness, but no consistency
- type level programming: flexible, but excessive
- property: convinient, but lack of consistency?
- lazy argument: not essential
- subtyping: not essential
- unify "let" and "var"

# Memo
https://www.swift.org/
https://www.haskell.org/onlinereport/syntax-iso.html
https://v2.ocaml.org/manual/expr.html
https://github.com/fsharp/fslang-design
