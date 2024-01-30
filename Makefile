watch:
	-make t
	-fswatch -0 -o -l 2 src test | xargs -I {} -n1 -0 make t

t:
	clear
	cat test/*.moa | node src/moa.js
# log
	echo '(log 1)'                   | node src/moa.js 2>&1 | grep -qx 1
# assert
	echo '(assert 1 1)'              | node src/moa.js
	! echo '(assert 1 2)'            | node src/moa.js 2> /dev/null
# throw / catch
	echo '(catch (throw "a") log)'   | node src/moa.js 2> /dev/null
	! echo '(throw "a")'             | node src/moa.js 2> /dev/null
# comment
	echo '(var a 1)\n#b\n(log a)'    | node src/moa.js 2>&1 | grep -qx 1
	echo '(var a 1) \n #b \n(log a)' | node src/moa.js 2>&1 | grep -qx 1
	@echo ok

repl:
	node src/moa.js

wc:
	wc src/moa.js
	wc test/*.moa
