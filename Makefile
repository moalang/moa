test:
	@-clear
	@-(cd src && node bootstrap.js test)

mini:
	@-clear
	@-(cd src && node bootstrap.js test && minimoa mini.moa)

watch:
	@make mini
	@-fswatch -0 -o . | xargs -I {} -n1 -0 make mini
