command=test

help:
	@clear
	@node main.js help

run_test:
	@clear
	@node main.js test

run_js:
	@clear
	@node main.js js < v1.moa > /tmp/moa.js
	@node /tmp/moa.js

test:
	@make watch command=run_test

js:
	@make watch command=run_js

watch:
	@make ${command}
	@fswatch -0 -o . | xargs -I {} -n1 -0 make ${command}
