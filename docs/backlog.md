# Backlog
- [x] Define syntax
- [x] Define core
- [x] Define test
- [ ] Implement "var" and "let" with initialization statement

# Ideas
- [ ] Unify "let" and "var"
- [ ] Define "while"
- [ ] Define property
- [ ] Define fallback to optional   , e.g., "a[n]?"  -> "catch(some(a[n]) _ => none)"
- [ ] Define fallback to alternative, e.g., "a[n]?0" -> "catch(some(a[n]) _ => 0)"
- [ ] Define fallback to zero value , e.g., "a[n]??" -> "catch(a[n] _[t] => t.zero)"
- [ ] Define to do if no exception  , e.g., "when v = a[0]: log(n)"
- [ ] Define lazy evaluated argument
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
- [ ] Define optional while
- [ ] Define checked and unchecked for operator of sized integers

# Allowed
- exception: for unified error handling
- variadic arguments: for "print" and function wrapper
- optional: for input and output
- overload: for property and method type inference, functions

# Rejected
- number interface: "**" is not good match for some types, e.g, vector2
- ";": ambiguous in class and enum, harder parsing arguments
- optional argument: low frequency
- default argument: low frequency

# Pending
- "for": might be replaced with seq(n).each
- "each": might be replaced with list.each
- ordered dict: convinient, but slower
- property: no consistency, but intuitiveness
- type level programming: flexible, but excessive

# Memo
https://www.swift.org/
https://www.haskell.org/onlinereport/syntax-iso.html
https://v2.ocaml.org/manual/expr.html
https://github.com/fsharp/fslang-design
