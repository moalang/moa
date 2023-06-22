# moa.mod
`moa.mod` describes dependencie.
The format of line is `name version location?`.
The package is expose as `name` in the project.
If `version` is not existed, the compiler will write a specific version.

## Example
moa: 1.1
module: mypackage
version: 1.2.3
require:
-  package1 1.3 ../submodules/package1/moa.mod
-  package2 1.4 https://example.com/package2/src/moa.mod
