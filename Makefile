test:
	@-clear
	@-(cd src && node bootstrap.js test && minimoa mini.moa)

watch:
	@make test
	@-fswatch -0 -o . | xargs -I {} -n1 -0 make test
