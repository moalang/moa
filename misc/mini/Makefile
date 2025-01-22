test:
	echo '(def main (((. io puts) 123)))' | node bootstrap.js | grep 123
	echo '(def main (((. io puts) "hello world")))' | node bootstrap.js | grep "hello world"
