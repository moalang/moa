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

html:
	@make watch command=run_html

run_test:
	@clear && printf '\e[3J'
	@node bootstrap.js test

run_js:
	@clear && printf '\e[3J'
	@cat runtime.js > /tmp/moa.js
	@node bootstrap.js js < moa.moa >> /tmp/moa.js
	@echo "io.reads = () => require('fs').readFileSync('/dev/stdin', 'utf8')" >> /tmp/moa.js
	@echo 'console.error(main())' >> /tmp/moa.js
	@cat -n /tmp/moa.js
	@node /tmp/moa.js < moa.moa

run_html:
	@clear && printf '\e[3J'
	@echo '<script>' > /tmp/moa.html
	@node bootstrap.js js < moa.moa >> /tmp/moa.html
	@open /tmp/moa.html
