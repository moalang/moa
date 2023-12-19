watch:
	@make test
	fswatch -0 -o -l 2 src/*.* | xargs -I {} -n1 -0 make test

test:
	clear
	-node src/parser.js
	#node src/interpriter.js
	#-node src/bootstrap.js

mc:
	node misc/mc src/*
