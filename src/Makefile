command=test

help:
	@clear
	@runghc Main.hs help

run_test:
	@clear
	@runghc Main.hs test

run_js:
	@clear
	@runghc Main.hs js < js.moa

test:
	@make watch command=run_test

js:
	@make watch command=run_js

watch:
	@make ${command}
	@fswatch -0 -o . | xargs -I {} -n1 -0 make ${command}
