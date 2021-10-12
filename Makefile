test:
	@-printf '\033\143'
	@-(cd src && node bootstrap.js test)

watch-test:
	@make test
	@-fswatch -0 -o . | xargs -I {} -n1 -0 make test

mini:
	@-printf '\033\143'
	@-(cd src && node bootstrap.js test && minimoa mini.moa)

watch-mini:
	@make mini
	@-fswatch -0 -o . | xargs -I {} -n1 -0 make mini
