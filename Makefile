moa:
	(cd src && go build -o ~/local/bin/moa main.go)
	moa version

boot:
	(cd src && node bootstrap.js js < moa.moa > /tmp/a.js)
	(cd src && node /tmp/a.js < moa.moa > /tmp/a.go)
	(cd src && echo 'def main io.puts("hello bootstrap")' | go run /tmp/a.go)

test:
	(cd src && node bootstrap.js test)
	(cd src && echo 'def main io.puts("boot")' | node /tmp/a.js > /tmp/a.go && go run /tmp/a.go | grep boot)
