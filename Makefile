moa:
	(cd src && go build -o ~/local/bin/moa main.go)
	moa version

test:
	(cd src && node bootstrap.js test)
