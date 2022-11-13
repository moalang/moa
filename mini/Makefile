test:
	clear
	-node parse.js
	-node infer.js

watch:
	-make test
	fswatch -0 parse.js infer.js | xargs -I {} -n1 -0 make
