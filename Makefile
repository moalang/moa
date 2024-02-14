watch:
	-make t
	-fswatch -0 -o -l 2 Makefile src test | xargs -I {} -n1 -0 make t

t:
	clear
	cat test/*.moa | node src/moa.js
### arguments from shell
##	echo 'assert list("a" "b") io.argv' | node src/moa.js a b 2>&1 > /dev/null
### multiline string
##	echo 'assert "a\\nb" "a\nb"'    | node src/moa.js 2>&1 > /dev/null
### assert with block
##	echo 'assert 1:\n  def f: 1\n  f()' | node src/moa.js 2>&1 > /dev/null
### exit code
##	! echo 'assert 1 2'             | node src/moa.js 2>&1 > /dev/null
##	! echo 'throw "a"'              | node src/moa.js 2>&1 > /dev/null
### log & comment
##	echo '#a\n1\n #b \nlog 1#c\n#d' | node src/moa.js 2>&1 | grep -qx 1
### syntax error
##	echo 'abc'                 | node src/moa.js 2>&1 | grep -q 'can not find value `abc` in this scope'
##	echo 'abc += 1'            | node src/moa.js 2>&1 | grep -q 'can not find value `abc` in this scope'
### stdio
##	echo 'io.print io.stdin.utf8' | node src/moa.js | grep -q 'io.print io.stdin.utf8'
### go
##	echo 'def main: io.print "Hello go"' | node src/moa.js go > /tmp/a.go && go run /tmp/a.go | grep -q 'Hello go'
### self boot
##	(cd src && echo 'def main: io.print "Hello moa"' | node moa.js build) > /tmp/a.go && go run /tmp/a.go | grep -q 'Hello moa'
### type hint
##	(cd src && echo 'dec add int int int\ndef add a b: a + b\ndef main: io.print add(1 2)' | node moa.js build) > /tmp/a.go && go run /tmp/a.go | grep -q 3
	@echo ok

s:
	(cd src && echo 'def main: io.print "Hello moa"' | node moa.js build) > /tmp/a.go && go run /tmp/a.go | grep -q 'Hello moa'

r:
	node src/moa.js

w:
	wc src/moa.js
	wc test/*.moa
