command=test

help:
	clear
	runghc Main.hs help

test:
	clear
	runghc Main.hs test

go:
	clear
	runghc Main.hs compile < go.moa

repl:
	runghc Main.hs repl

watch:
	-make ${command}
	fswatch -0 -o . | xargs -I {} -n1 -0 make ${command}
