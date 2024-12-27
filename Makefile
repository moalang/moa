TEST_SCRIPTS := $(wildcard test/*.sh)

moa:
	(cd src && go build -o ~/local/bin/moa main.go)
	moa version

.PHONY: test
test: $(TEST_SCRIPTS)
	mkdir -p /tmp/moa_test
	(cd src && go test -v main.go main_test.go && go build -o /tmp/moa_test/moa main.go)
	@for script in $^; do bash $$script || exit 1; done
