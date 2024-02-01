watch:
	-make t
	-fswatch -0 -o -l 2 src test | xargs -I {} -n1 -0 make t

t:
	clear
	grep -h assert test/* | node src/moa.js
# exit code
	! echo '(assert 1 2)'             | node src/moa.js 2> /dev/null
	! echo '(throw "a")'              | node src/moa.js 2> /dev/null
# log & comment
	echo '#a\n1\n #b \n(log 1)#c\n#d' | node src/moa.js 2>&1 | grep -qx 1
	@echo ok

r:
	node src/moa.js

w:
	wc src/moa.js
	wc test/*.moa
