run:
	@printf '\033\143'
	@(cd src && time node moa.js --test)
	@echo '#!node\n' > bin/moa
	@cat src/moa.js >> bin/moa

watch:
	@-make run
	@-fswatch -0 -o src/moa.js src/moa.moa | xargs -I {} -n1 -0 make run
