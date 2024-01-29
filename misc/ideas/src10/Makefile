test:
	@-clear && printf '\e[3J'
	@-node bootstrap.js test
	@-echo "main=io.exit(0)" | node bootstrap.js | lli
	@-node test.js

watch:
	@make test
	@-fswatch -0 -o . | xargs -I {} -n1 -0 make test
