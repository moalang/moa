watch:
	-make test
	-fswatch -0 -o -l 2 Makefile src | xargs -I {} -n1 -0 make test

test:
	clear
# feature
	echo '(log "hello world")'                    | node src/moa.js 2>&1 | grep -qx 'hello world'
	echo '(log (-1))'                             | node src/moa.js 2>&1 | grep -qx -- -1
	echo '(log (-1).abs)'                         | node src/moa.js 2>&1 | grep -qx 1
	echo '(log 1.neg)'                            | node src/moa.js 2>&1 | grep -qx -- -1
	echo '(def a (b c) (+ b c)) (log (a 1 2))'    | node src/moa.js 2>&1 | grep -qx 3
	echo '(var a 1) (+= a 2) (log a)'             | node src/moa.js 2>&1 | grep -qx 3
	echo '(struct a ((b int))) (log (. (a 1) b))' | node src/moa.js 2>&1 | grep -qx 1
	echo '(log (list))'                           | node src/moa.js 2>&1 | grep -qx '(list)'
	echo '(log (list 1))'                         | node src/moa.js 2>&1 | grep -qx '(list 1)'
	echo '(log (dict 1 2))'                       | node src/moa.js 2>&1 | grep -qx '(dict 1 2)'
	echo '(log 1)'                                | node src/moa.js 2>&1 | grep -qx 1
	echo '(log (! true))'                         | node src/moa.js 2>&1 | grep -qx false
	echo '(log (+ 1 2 3))'                        | node src/moa.js 2>&1 | grep -qx 6
	echo '(log (- 3 2))'                          | node src/moa.js 2>&1 | grep -qx 1
	echo '(log (* 2 3))'                          | node src/moa.js 2>&1 | grep -qx 6
	echo '(log (/ 4 2))'                          | node src/moa.js 2>&1 | grep -qx 2
	echo '(log (% 5 2))'                          | node src/moa.js 2>&1 | grep -qx 1
	echo '(log (| 1 2))'                          | node src/moa.js 2>&1 | grep -qx 3
	echo '(log (& 3 2))'                          | node src/moa.js 2>&1 | grep -qx 2
	echo '(log (^ 3 2))'                          | node src/moa.js 2>&1 | grep -qx 1
	echo '(log (** 2 3))'                         | node src/moa.js 2>&1 | grep -qx 8
	echo '(log (|| false false))'                 | node src/moa.js 2>&1 | grep -qx false
	echo '(log (&& true true))'                   | node src/moa.js 2>&1 | grep -qx true
	echo '(log (1 == 1))'                         | node src/moa.js 2>&1 | grep -qx true
	echo '(log (1 != 1))'                         | node src/moa.js 2>&1 | grep -qx false
	echo '(log (1 > 1))'                          | node src/moa.js 2>&1 | grep -qx false
	echo '(log (1 >= 1))'                         | node src/moa.js 2>&1 | grep -qx true
	echo '(log (1 < 1))'                          | node src/moa.js 2>&1 | grep -qx false
	echo '(log (1 <= 1))'                         | node src/moa.js 2>&1 | grep -qx true
	echo '(log (++ (list 1) (list 2))'            | node src/moa.js 2>&1 | grep -qx '(list 1 2)'
	echo '(log (++ (dict 1 2) (dict 3 4))'        | node src/moa.js 2>&1 | grep -qx '(dict 1 2 3 4)'
# methods
	echo '(log "hi".size)'     | node src/moa.js 2>&1 | grep -qx 2
	echo '(log (list 1).size)' | node src/moa.js 2>&1 | grep -qx 1
# comment
	echo '(var a 1)\n#b\n(log a)'    | node src/moa.js 2>&1 | grep -qx 1
	echo '(var a 1) \n #b \n(log a)' | node src/moa.js 2>&1 | grep -qx 1
# syntax sugar
# a b       -> (a b)
	echo 'log 1' | node src/moa.js 2>&1 | grep -qx 1
	echo 'log !true'               | node src/moa.js 2>&1 | grep -qx false
# a + b     -> (+ a b)
	echo 'log 1 + 2' | node src/moa.js 2>&1 | grep -qx 3
# a+b     -> (+ a b)
	echo 'log 1+2' | node src/moa.js 2>&1 | grep -qx 3
# a + b * c -> (+ a (* b c))
	echo 'log 1 + 2 * 3'      | node src/moa.js 2>&1 | grep -qx 7
	echo 'log 1 + 2 == 7 - 4' | node src/moa.js 2>&1 | grep -qx true
# a b\nc d  -> (a b) (c d)
	echo 'let a 1\nlog a' | node src/moa.js 2>&1 | grep -qx 1
# a b; c d  -> (a b) (c d)
	echo 'let a 1;log a' | node src/moa.js 2>&1 | grep -qx 1
# {a}       -> {a}
	echo '{log 1}' | node src/moa.js 2>&1 | grep -qx 1
# {a; b c}  -> {a (b c)}
	echo '{let a 1; log a}' | node src/moa.js 2>&1 | grep -qx 1
# a()       -> (a)
# a(b)      -> (a b)
# a.b()     -> ((. a b))
# a.b(c)    -> ((. a b) c)
# edge cases
	echo '1; log 2'                              | node src/moa.js 2>&1 | grep -qx 2
	echo '{1; log 2}'                            | node src/moa.js 2>&1 | grep -qx 2
	echo '{{1; log 2}}'                          | node src/moa.js 2>&1 | grep -qx 2
	echo 'def a (b) {b += 1; b += 2}; log (a 1)' | node src/moa.js 2>&1 | grep -qx 4
	echo 'def a (b) {b += 1; b += 2}\nlog (a 1)' | node src/moa.js 2>&1 | grep -qx 4
	echo 'def a (b c) b + c; log (a 1 2)'        | node src/moa.js 2>&1 | grep -qx 3
	@echo ok

mc:
	node misc/mc src/*
