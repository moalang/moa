watch:
	@make test
	fswatch -0 -o -l 2 src/*.js | xargs -I {} -n1 -0 make test

test:
	clear
	node src/parser.js
	node src/interpriter.js
	-node src/bootstrap.js
