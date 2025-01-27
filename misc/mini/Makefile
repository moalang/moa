test:
	echo '(def main (((. io puts) (+ 1 2))))' | node bootstrap.js | grep 3
	# Syntax sugar
	echo 'def main (((. io puts) (+ 1 2)))' | node bootstrap.js | grep 3 # (a b) -> a b
	echo 'def main ((io.puts (+ 1 2)))'     | node bootstrap.js | grep 3 # (. a b) -> a.b
	echo 'def main ((io.puts 1 + 2))'       | node bootstrap.js | grep 3 # (+ a b) -> a + b
	echo 'def main: io.puts 1 + 2'          | node bootstrap.js | grep 3 # def a ((b c)) -> def a: b c
	echo 'def main:\n let a 3\n io.puts a'  | node bootstrap.js | grep 3 # def a ((b c) (d f)) -> def a:\n b c\n d f
