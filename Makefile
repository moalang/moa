test:
	@-printf '\033\143'
	@-(cd src && node bootstrap.js test)

watch:
	@make test
	@-fswatch -0 -o . | xargs -I {} -n1 -0 make test

bootstrap:
	@-printf '\033\143'
	@-(cd src && node bootstrap.js bootstrap)
