TEST_SCRIPTS := $(wildcard test/*.sh)

moa:
	(cd src && go build -o ~/local/bin/moa .)

.PHONY: test
test: $(TEST_SCRIPTS)
	mkdir -p /tmp/moa_test
	(cd src && go test -v && go build -o /tmp/moa_test/moa .)
	@for script in $^; do bash $$script || exit 1; done
