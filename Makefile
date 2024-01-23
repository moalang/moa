watch:
	-make test
	-fswatch -0 -o -l 2 src/*.* | xargs -I {} -n1 -0 make test

test:
	clear
	echo "(print 1)" | node src/moa.js | grep -x 1
	echo "(print (+ 1 2 3))" | node src/moa.js | grep -x 6
	echo "(print (- 3 2))" | node src/moa.js | grep -x 1
	echo "(print (* 2 3))" | node src/moa.js | grep -x 6
	echo "(print (/ 4 2))" | node src/moa.js | grep -x 2
	echo "(print (% 5 2))" | node src/moa.js | grep -x 1
	echo "(print (** 2 3))" | node src/moa.js | grep -x 8
	echo "(print (|| false false))" | node src/moa.js | grep -x false
	echo "(print (&& true true))" | node src/moa.js | grep -x true
	echo "(def add (a b) (+ a b)) (print (add 1 2))" | node src/moa.js | grep -x 3
	echo "(var a 1) (+= a 2) (print a)" | node src/moa.js | grep -x 3
	echo "(struct a ((b int))) (print (. (a 1) b))" | node src/moa.js | grep -x 1
	echo "(print 1 + 2 * 3)" | node src/moa.js | grep -x 7

mc:
	node misc/mc src/*
