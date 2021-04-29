test:
	@-clear && printf '\e[3J'
	@-node bootstrap.js

watch:
	@make test
	@-fswatch -0 -o . | xargs -I {} -n1 -0 make test
