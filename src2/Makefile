run:
	clear
	@runghc Main

mac:
	@-make
	@-fswatch -0 -o . | xargs -I {} -n1 -0 make
