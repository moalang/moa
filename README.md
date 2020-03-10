# The Moa Programming Language
Moa is an open source programming language that makes programming fun.

## Simple language
- Values: bool, int, float, string, closure, coroutine
- Containers: array, dictionary
- Types: struct, enum, interface, generics
- Statement: sequence, branch
- Permissions: mutable, imutable
- Error handling: error(T)
- Namespace

## Easy to use
- REPL
- Hot code reloading
- Cross compilation to Linux, Windows and Mac

## Performance
- As fast as C based on LLVM
- No GC

## Safety
- No null
- No exception
- No shadowing
- No global variables
- No implicit type casting
- No undefined specification
- Array bounds checking
- Any mutability and IO are only inside monad

## Features of the future
- Ownership
- Worker and transactional shared database
- Preconditions and Postconditions

## The expression for optional, error and side effect
opt a: some a value | none
try a: ok a value | fail message string, params ...
eff a: _

Syntax sugar.
```
<-   # try + eff?
exp! # try + eff?

# opt
head a : [a] a?
head xs = xs.0.rescue(_ -> none)
convert : [num] num?
convert xs = head(xs)?abs ?+ 1
calc : [num] num
calc xs = convert(xs).alt(0)
# try
head a : [a] a!
head xs = xs.0
convert : [num] num!
convert xs = head(xs)!abs !+ 1
calc : [num] num
calc xs = convert(xs).rescue(e -> 0)
# eff
count var(n)
incr : int&
incr = count += 1
main : int&
main = incr; incr
```
