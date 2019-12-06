test:
	clear
	runghc Main.hs test

repl:
	runghc Main.hs

watch:
	-make test
	fswatch -0 -o . | xargs -I {} -n1 -0 make test
