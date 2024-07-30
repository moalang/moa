# Backlog
- [x] Define syntax
- [x] Define core
- [x] Define test
- [ ] Implement optional argument
- [ ] Define "for"
- [ ] Define "each"
- [ ] Define fallback to optional   , e.g., "a[n]?"  -> "catch(some(a[n]) _ => none)"
- [ ] Define fallback to alternative, e.g., "a[n]?0" -> "catch(some(a[n]) _ => 0)"
- [ ] Define fallback to zero value , e.g., "a[n]??" -> "catch(a[n] _[t] => t.zero)"
- [ ] Define to do if no exception  , e.g., "when v = a[0]: log(n)"
- [ ] Compile to JavaScript
- [ ] Compile to Go
- [ ] Define method
- [ ] Define typed argument
- [ ] Define subtyping like TypeScript
- [ ] UI framework for compiled JavaScript
- [ ] Backend framework for compiled Go
- [ ] Web IDE
- [ ] Define standard libraries

# Allowed
- exception: for unified error handling
- variadic arguments: for "print" or function wrapper
- optional: for user input

# Rejected
- number interface: "**" is not good match for some types, e.g, vector2

# Pending
- ordered dict: convinient, but slower
- overload: it reduces readability, but IDE could mitigate?
- property: no consistency, but intuitiveness
- type level programming: flexible, but excessive

# Memo
https://www.swift.org/
https://www.haskell.org/onlinereport/syntax-iso.html
https://v2.ocaml.org/manual/expr.html
https://github.com/fsharp/fslang-design
