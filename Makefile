test:
	@printf '\033\143'
	@(cd src && node bootstrap.js)
	@node bin/moa checkup && node bin/moa src/moa.moa

watch:
	@-make test
	@-fswatch -0 -o src/bootstrap.js src/moa.moa | xargs -I {} -n1 -0 make test
