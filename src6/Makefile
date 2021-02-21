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
	@node bootstrap.js test

run_js:
	@clear && printf '\e[3J'
	@node bootstrap.js js < moa.moa > /tmp/moa.js
	@node /tmp/moa.js < moa.moa
