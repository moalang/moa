command=test

help:
	@clear
	@node main.js help

watch:
	@-make ${command}
	@-fswatch -0 -o . | xargs -I {} -n1 -0 make ${command}

test:
	@make watch command=run_test

js:
	@make watch command=run_js

run_test:
	@clear
	@node main.js test

run_js:
	@clear
	@node main.js js < v1.moa > /tmp/moa.js
	@node main.js html < v1.moa > /tmp/moa.html
	@node /tmp/moa.js
