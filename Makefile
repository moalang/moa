watch:
	-make test
	-fswatch -0 -o -l 2 Makefile src/*.* | xargs -I {} -n1 -0 make test

test:
	clear
# feature
	echo '(print "hello world")'                    | node src/moa.js | grep -qx 'hello world'
	echo '(def a (b c) (+ b c)) (print (a 1 2))'    | node src/moa.js | grep -qx 3
	echo '(var a 1) (+= a 2) (print a)'             | node src/moa.js | grep -qx 3
	echo '(struct a ((b int))) (print (. (a 1) b))' | node src/moa.js | grep -qx 1
	echo '(print (list))'                           | node src/moa.js | grep -qx '(list)'
	echo '(print (list 1))'                         | node src/moa.js | grep -qx '(list 1)'
	echo '(print (dict 1 2))'                       | node src/moa.js | grep -qx '(dict 1 2)'
	echo '(print 1)'                                | node src/moa.js | grep -qx 1
	echo '(print !true)'                            | node src/moa.js | grep -qx false
	echo '(print (+ 1 2 3))'                        | node src/moa.js | grep -qx 6
	echo '(print (- 3 2))'                          | node src/moa.js | grep -qx 1
	echo '(print (* 2 3))'                          | node src/moa.js | grep -qx 6
	echo '(print (/ 4 2))'                          | node src/moa.js | grep -qx 2
	echo '(print (% 5 2))'                          | node src/moa.js | grep -qx 1
	echo '(print (** 2 3))'                         | node src/moa.js | grep -qx 8
	echo '(print (|| false false))'                 | node src/moa.js | grep -qx false
	echo '(print (&& true true))'                   | node src/moa.js | grep -qx true
	echo '(print (1 == 1))'                         | node src/moa.js | grep -qx true
	echo '(print (1 != 1))'                         | node src/moa.js | grep -qx false
	echo '(print (1 > 1))'                          | node src/moa.js | grep -qx false
	echo '(print (1 >= 1))'                         | node src/moa.js | grep -qx true
	echo '(print (1 < 1))'                          | node src/moa.js | grep -qx false
	echo '(print (1 <= 1))'                         | node src/moa.js | grep -qx true
	echo '(print (++ (list 1) (list 2))'            | node src/moa.js | grep -qx '(list 1 2)'
	echo '(print (++ (dict 1 2) (dict 3 4))'        | node src/moa.js | grep -qx '(dict 1 2 3 4)'
# methods
	echo '(print "hi".size)'     | node src/moa.js | grep -qx 2
	echo '(print (list 1).size)' | node src/moa.js | grep -qx 1
# comment
	echo '(var a 1)\n#b\n(print a)'    | node src/moa.js | grep -qx 1
	echo '(var a 1) \n #b \n(print a)' | node src/moa.js | grep -qx 1
# syntax sugar
# a b       -> (a b)
	echo 'print 1' | node src/moa.js | grep -qx 1
# a + b     -> (+ a b)
	echo 'print 1 + 2' | node src/moa.js | grep -qx 3
# a+b     -> (+ a b)
	echo 'print 1+2' | node src/moa.js | grep -qx 3
# a + b * c -> (+ a (* b c))
	echo 'print 1 + 2 * 3' | node src/moa.js | grep -qx 7
# a b\nc d  -> (a b) (c d)
	echo 'let a 1\nprint a' | node src/moa.js | grep -qx 1
# a b; c d  -> (a b) (c d)
	echo 'let a 1;print a' | node src/moa.js | grep -qx 1
# {a}       -> {a}
	echo '{print 1}' | node src/moa.js | grep -qx 1
# {a; b c}  -> {a (b c)}
	echo '{let a 1; print a}' | node src/moa.js | grep -qx 1
# a()       -> (a)
# a(b)      -> (a b)
# a.b()     -> ((. a b))
# a.b(c)    -> ((. a b) c)
# edge cases
	echo '1; print 2'                              | node src/moa.js | grep -qx 2
	echo '{1; print 2}'                            | node src/moa.js | grep -qx 2
	echo '{{1; print 2}}'                          | node src/moa.js | grep -qx 2
	echo 'def a (b) {b += 1; b += 2}; print (a 1)' | node src/moa.js | grep -qx 4
	echo 'def a (b) {b += 1; b += 2}\nprint (a 1)' | node src/moa.js | grep -qx 4
	echo 'def a (b c) b + c; print (a 1 2)'        | node src/moa.js | grep -qx 3
	@echo ok

mc:
	node misc/mc src/*
