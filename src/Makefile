TARGET=test

test:
	clear
	runghc Main.hs test

go:
	clear
	runghc Main.hs < go.moa

repl:
	runghc Main.hs repl

watch:
	-make ${TARGET}
	fswatch -0 -o . | xargs -I {} -n1 -0 make test
