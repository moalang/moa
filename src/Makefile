test:
	@-clear && printf '\e[3J'
	@-node bootstrap.js test

watch:
	@make test
	@-fswatch -0 -o . | xargs -I {} -n1 -0 make test
