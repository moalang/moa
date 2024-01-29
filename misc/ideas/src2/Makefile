run:
	clear
	@PATH=/usr/local/opt/node@13/bin/:$$PATH time runghc Main

mac:
	@-make
	@-fswatch -0 -o . | xargs -I {} -n1 -0 make
