test:
	@-clear
	@-node bootstrap.js test && moa run mini.moa < mini.moa

watch:
	@make test
	@-fswatch -0 -o . | xargs -I {} -n1 -0 make test
