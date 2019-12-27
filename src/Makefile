command=test

help:
	clear
	runghc Main.hs help

test:
	clear
	runghc Main.hs test

go:
	clear
	runghc Main.hs go < go.moa

watch:
	-make ${command}
	fswatch -0 -o . | xargs -I {} -n1 -0 make ${command}
