# Table of signatures

## add int int int
C      : int add(x int, y int) { ... }
OCaml  : val add : float -> float -> float
Haskell: add :: int int int
Moa    : add : int int int

## id a : a a
OCaml  : id : 'a -> 'a
Haskell: id :: a a
Moa    : id a : a a

## type definition and signature
person: name string; age int
vector3: x,y,z int
add : int int int
id a : a a
