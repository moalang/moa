all:
	# test runtime
	go test -v runtime.go runtime_test.go
	# test bootstrap
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
	echo 'struct v1: x int\ndef main: io.puts v1(3).x'            | node bootstrap.js | grep 3 # struct
	echo 'def add a b: return a + b\ndef main: io.puts add(1 2)'  | node bootstrap.js | grep 3 # type inference: function
	echo 'def main: io.puts add(1 2)\ndef add a b: return a + b'  | node bootstrap.js | grep 3 # type inference: function
	echo 'def main:\n let a []\n a.push(1)\n io.puts(a.size())'   | node bootstrap.js | grep 1 # type inference
	# test moa.moa with bootstrap
	cat moa.moa | node bootstrap.js run io.puts 3
