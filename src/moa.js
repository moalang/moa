const compile_to_js = (src) => src
let a = process.argv[2]
if (a === 'build') {
  let fs = require('fs')
  let src = fs.readFileSync(process.argv[3], 'utf8')
  console.log(compile_to_js(src))
} else if (a === 'version') {
  console.log('moa0.0.1 js')
} else {
  console.log(`Moa is a tool for managing Moa source code.

Usage:

  moa <command> [arguments]

The commands are:

	build       compile packages and dependencies
	version     print Moa version

TBD:

	doc         show documentation for package or symbol
	env         print Moa environment information
	fix         update packages to use new APIs
	fmt         gofmt (reformat) package sources
	install     compile and install packages and dependencies
	run         compile and run Moa program
	test        test packages`)
}
