test:
	@node bootstrap.js test

watch:
	make test
	@-fswatch -0 -l 0.1 -o . | xargs -I {} -0 make test
