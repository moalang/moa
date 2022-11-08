test:
	clear
	-node parse.js

watch:
	-make test
	fswatch -0 parse.js | xargs -I {} -n1 -0 make
