watch:
	-make test
	-fswatch -0 -o -l 2 Makefile src | xargs -I {} -n1 -0 make test

test:
	clear
# bool
	echo '(log true)'                             | node src/moa.js 2>&1 | grep -qx true
	echo '(log (! false))'                        | node src/moa.js 2>&1 | grep -qx true
	echo '(log (|| false false))'                 | node src/moa.js 2>&1 | grep -qx false
	echo '(log (&& true true))'                   | node src/moa.js 2>&1 | grep -qx true
# number
	echo '(log 1)'                                | node src/moa.js 2>&1 | grep -qx 1
	echo '(log 1.2)'                              | node src/moa.js 2>&1 | grep -qx 1.2
	echo '(log (-1))'                             | node src/moa.js 2>&1 | grep -qx -- -1
	echo '(log (+ 1 2 3))'                        | node src/moa.js 2>&1 | grep -qx 6
	echo '(log (- 3 2))'                          | node src/moa.js 2>&1 | grep -qx 1
	echo '(log (* 2 3))'                          | node src/moa.js 2>&1 | grep -qx 6
	echo '(log (/ 4 2))'                          | node src/moa.js 2>&1 | grep -qx 2
	echo '(log (% 5 2))'                          | node src/moa.js 2>&1 | grep -qx 1
	echo '(log (| 1 2))'                          | node src/moa.js 2>&1 | grep -qx 3
	echo '(log (& 3 2))'                          | node src/moa.js 2>&1 | grep -qx 2
	echo '(log (^ 3 2))'                          | node src/moa.js 2>&1 | grep -qx 1
	echo '(log (** 2 3))'                         | node src/moa.js 2>&1 | grep -qx 8
	echo '(log (1 == 1))'                         | node src/moa.js 2>&1 | grep -qx true
	echo '(log (1 != 1))'                         | node src/moa.js 2>&1 | grep -qx false
	echo '(log (1 > 1))'                          | node src/moa.js 2>&1 | grep -qx false
	echo '(log (1 >= 1))'                         | node src/moa.js 2>&1 | grep -qx true
	echo '(log (1 < 1))'                          | node src/moa.js 2>&1 | grep -qx false
	echo '(log (1 <= 1))'                         | node src/moa.js 2>&1 | grep -qx true
	echo '(log (. (-1) abs))'                     | node src/moa.js 2>&1 | grep -qx 1
	echo '(log (. 1 neg))'                        | node src/moa.js 2>&1 | grep -qx -- -1
	echo '(log (. 65 char))'                      | node src/moa.js 2>&1 | grep -qx A
	echo '(log (. 1.9 floor))'                    | node src/moa.js 2>&1 | grep -qx 1
	echo '(log (. 1.1 ceil))'                     | node src/moa.js 2>&1 | grep -qx 2
	echo '(log (. 1.4 round))'                    | node src/moa.js 2>&1 | grep -qx 1
	echo '(log (. 1.5 round))'                    | node src/moa.js 2>&1 | grep -qx 2
# string
	echo '(log "")'                               | node src/moa.js 2>&1 | grep -qx ''
	echo '(log (++ "a" "b")'                      | node src/moa.js 2>&1 | grep -qx ab
	echo '(log (. "ab" size))'                    | node src/moa.js 2>&1 | grep -qx 2
	echo '(log (. "ab" reverse))'                 | node src/moa.js 2>&1 | grep -qx 'ba'
	echo '(log ((. "aba" slice) 1))'              | node src/moa.js 2>&1 | grep -qx ba
	echo '(log ((. "aba" slice) 1 2))'            | node src/moa.js 2>&1 | grep -qx b
	echo '(log ((. "aba" replace) "a" "_"))'      | node src/moa.js 2>&1 | grep -qx '_b_'
# list
	echo '(log (list))'                           | node src/moa.js 2>&1 | grep -qx '(list)'
	echo '(log (list 1))'                         | node src/moa.js 2>&1 | grep -qx '(list 1)'
	echo '(log (++ (list 1) (list 2))'            | node src/moa.js 2>&1 | grep -qx '(list 1 2)'
	echo '(log (. (list 1) size))'                | node src/moa.js 2>&1 | grep -qx 1
# set
	echo '(log (set))'                            | node src/moa.js 2>&1 | grep -qx '(set)'
	echo '(log (set 1 1))'                        | node src/moa.js 2>&1 | grep -qx '(set 1)'
	echo '(log (. (set 1) size))'                 | node src/moa.js 2>&1 | grep -qx 1
	echo '(log ((. (set 1) has) 1))'              | node src/moa.js 2>&1 | grep -qx true
	echo '(log ((. (set 1) add) 1))'              | node src/moa.js 2>&1 | grep -qx false
	echo '(log ((. (set 1) add) 2))'              | node src/moa.js 2>&1 | grep -qx true
	echo '(log ((. (set 1) rid) 1))'              | node src/moa.js 2>&1 | grep -qx true
	echo '(log ((. (set 1) rid) 2))'              | node src/moa.js 2>&1 | grep -qx false
	echo '(log (- (set 1 2) (set 2))'             | node src/moa.js 2>&1 | grep -qx '(set 1)'
	echo '(log (| (set 1) (set 2))'               | node src/moa.js 2>&1 | grep -qx '(set 1 2)'
	echo '(log (& (set 1 2) (set 2))'             | node src/moa.js 2>&1 | grep -qx '(set 2)'
	echo '(log (^ (set 1) (set 1 2))'             | node src/moa.js 2>&1 | grep -qx '(set 2)'
	echo '(log (. (set 1) list)'                  | node src/moa.js 2>&1 | grep -qx '(list 1)'
# dict
	echo '(log (dict 1 2))'                       | node src/moa.js 2>&1 | grep -qx '(dict 1 2)'
	echo '(log (++ (dict 1 2) (dict 3 4))'        | node src/moa.js 2>&1 | grep -qx '(dict 1 2 3 4)'
	echo '(log (. (dict 1 2 3 4) size)'           | node src/moa.js 2>&1 | grep -qx 2
# variable
	echo '(var a 1) (+= a 2) (log a)'             | node src/moa.js 2>&1 | grep -qx 3
# function
	echo '(def a (b c) (+ b c)) (log (a 1 2))'    | node src/moa.js 2>&1 | grep -qx 3
# struct
	echo '(struct a ((b int))) (log (. (a 1) b))' | node src/moa.js 2>&1 | grep -qx 1
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
