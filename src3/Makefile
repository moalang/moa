command=test

help:
	@echo "make js"
	@echo "make test"

watch:
	@-make ${command}
	@-fswatch -0 -o . | xargs -I {} -n1 -0 make ${command}

test:
	@make watch command=run_test

js:
	@make watch command=run_js

run_test:
	@clear && printf '\e[3J'
	@node test.js

run_js:
	@clear && printf '\e[3J'
	@node moa.js < v1.moa > /tmp/moa.js
	@node /tmp/moa.js
