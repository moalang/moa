watch:
	-make test
	fswatch -0 *.js | xargs -I {} -n1 -0 make

test:
	clear
	-node parse.js
	-node convert.js
	-node compile.js
