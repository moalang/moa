watch:
	-make test
	-fswatch -0 -o -l 2 src/*.* | xargs -I {} -n1 -0 make test

test:
	clear
	echo "(log (+ 1 2))" | node src/moa.js | grep -sx 3
	echo "(log (- 3 2))" | node src/moa.js | grep -sx 1
	echo "(log (* 2 3))" | node src/moa.js | grep -sx 6
	echo "(log (/ 4 2))" | node src/moa.js | grep -sx 2
	echo "(log (% 5 2))" | node src/moa.js | grep -sx 1
	echo "(def add (a b) (+ a b)) (log (add 1 2))" | node src/moa.js | grep -sx 3
	echo "(var a 1) (+= a 2) (log a)" | node src/moa.js | grep -sx 3

mc:
	node misc/mc src/*
