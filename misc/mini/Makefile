r: # moa run
	cat moa.moa | node bootstrap.js run io.puts 3 | grep 3

t: # test
	echo '(def main (((. io puts) (+ 1 2))))' | node bootstrap.js   | grep 3 # lisp style
	echo 'def main (((. io puts) (+ 1 2)))'   | node bootstrap.js   | grep 3 # syntax sugar: (a b) -> a b
	echo 'def main ((io.puts (+ 1 2)))'       | node bootstrap.js   | grep 3 # syntax sugar: (. a b) -> a.b
	echo 'def main ((io.puts 1 + 2))'         | node bootstrap.js   | grep 3 # syntax sugar: (+ a b) -> a + b
	echo 'def main: io.puts 1 + 2'            | node bootstrap.js   | grep 3 # syntax sugar: def a ((b c)) -> def a: b c
	echo 'def main: io.puts(1 + 2)'           | node bootstrap.js   | grep 3 # syntax sugar: def a ((b c)) -> def a: b(c)
	echo 'def main:\n let a 3\n io.puts a'    | node bootstrap.js   | grep 3 # syntax sugar: def a ((b c) (d f)) -> def a:\n b c\n d f
	echo '# comment\ndef main: io.puts 3'     | node bootstrap.js   | grep 3 # ignore line comment
	echo 'def main: io.puts 3 # comment'      | node bootstrap.js   | grep 3 # ignore comment after expression
	echo 'def main: io.puts io.args.at(1)'    | node bootstrap.js 3 | grep 3 # io.args
