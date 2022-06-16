test:
	@printf '\033\143'
	@(cd src && time node moa.js --test)

watch:
	@-make test
	@-fswatch -0 -o src/moa.js src/moa.moa | xargs -I {} -n1 -0 make test
