s:
	cat test/*.moa | grep assert | egrep -v "\((set|time) " | node src/moa.js selftest
	#echo '(def main io.puts("Hello moa")' | node src/moa.js selfboot

watch:
	-make t
	-fswatch -0 -o -l 2 Makefile src test | xargs -I {} -n1 -0 make t

t:
	clear
	cat test/*.moa | grep assert | egrep -v "\((set|time) " | node src/moa.js
	@echo ok stage1
# multi-line string
	echo '(assert "a\\nb" "a\nb")' | node src/moa.js | grep -q .
# exit code
	! echo '(assert 1 2)' | node src/moa.js 2> /dev/null
	! echo '(throw "a")'  | node src/moa.js 2> /dev/null
# syntax error
	echo 'abc'      | node src/moa.js 2>&1 | grep -q 'not find value `abc` in this scope'
	echo 'abc += 1' | node src/moa.js 2>&1 | grep -q 'not find value `abc` in this scope'
# self boot
	echo '(def main io.puts("Hello moa")' | node src/moa.js selfboot | node | grep -q "Hello moa"
	@echo ok stage2

w:
	wc src/moa.js
	wc src/moa.moa
	wc test/*.moa
