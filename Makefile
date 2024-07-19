t test:
	clear
	node src/parse.js
	node src/infer.js
	node src/compile.js
	cat test/*.moa | head -n 5 | node src/moa.js

watch:
	-make t
	-fswatch -0 -o -l 2 Makefile src test | xargs -I {} -n1 -0 make t

wc:
	wc src/*
	wc test/*.moa
