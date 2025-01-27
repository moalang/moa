r: # test for runtime
	go test -v runtime.go runtime_test.go

b: # test for bootstrap
	echo '(def main (((. io puts) (+ 1 2))))'    | node bootstrap.js | grep 3 # lisp style
	echo 'def main (((. io puts) (+ 1 2)))'      | node bootstrap.js | grep 3 # syntax sugar: (a b) -> a b
	echo 'def main ((io.puts (+ 1 2)))'          | node bootstrap.js | grep 3 # syntax sugar: (. a b) -> a.b
	echo 'def main ((io.puts 1 + 2))'            | node bootstrap.js | grep 3 # syntax sugar: (+ a b) -> a + b
	echo 'def main: io.puts 1 + 2'               | node bootstrap.js | grep 3 # syntax sugar: def a ((b c)) -> def a: b c
	echo 'def main: io.puts(1 + 2)'              | node bootstrap.js | grep 3 # syntax sugar: def a ((b c)) -> def a: b(c)
	echo 'def main:\n let a 3\n io.puts a'       | node bootstrap.js | grep 3 # syntax sugar: def a ((b c) (d f)) -> def a:\n b c\n d f
	echo '# comment\ndef main: io.puts 3'        | node bootstrap.js | grep 3 # comment: line
	echo 'def main: io.puts 3 # comment'         | node bootstrap.js | grep 3 # comment: after expression
	echo 'def main: io.puts "hi!".size()'        | node bootstrap.js | grep 3 # string method
	echo 'def main: io.puts array(1 2 3).size()' | node bootstrap.js | grep 3 # array method
	echo 'def main: io.puts [1 2 3].size()'      | node bootstrap.js | grep 3 # syntax sugar: [a b] -> (array a b)
