s:
	make t

watch:
	-make t
	-fswatch -0 -o -l 2 Makefile src test | xargs -I {} -n1 -0 make t

t:
	clear
	node src/parser.js
	node src/infer.js
	#node src/compile.js
	#cat test/*.moa | node src/test.js

wc:
	wc src/*
	wc test/*.moa
