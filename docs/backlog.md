# Backlog
- [x] Define syntax
- [x] Define core
- [x] Define test

# Ideas
- [ ] Unify "let" and "var" to ":="
- [ ] Define "while"
- [ ] Compile to JavaScript
- [ ] Compile to Go
- [ ] Define method
- [ ] Define typed argument
- [ ] Define subtyping like TypeScript
- [ ] Define UI framework for compiled JavaScript
- [ ] Define Backend framework for compiled Go
- [ ] Make Web IDE
- [ ] Define named block
- [ ] Define standard libraries
- [ ] Define checked and unchecked for operator of sized integers

# Allowed
- variadic arguments: for "print" and function wrapper
- exception: for unified error handling
- option: for input and output
- overload: for property and method type inference, functions

# Rejected
- number interface: "**" is not good match for some types, e.g, vector2
- ";": ambiguous in class and enum, harder parsing arguments
- optional argument: low frequency
- default argument: low frequency
- covinient error handling: option should be more convinient

# Pending
- "for": might be replaced with seq(n).each
- "each": might be replaced with list.each
- ordered dict: convinient, but slower
- property: no consistency, but intuitiveness
- type level programming: flexible, but excessive
- property, lack of consistency?
- lazy evaluated argument, not necessary

# Memo
https://www.swift.org/
https://www.haskell.org/onlinereport/syntax-iso.html
https://v2.ocaml.org/manual/expr.html
https://github.com/fsharp/fslang-design
