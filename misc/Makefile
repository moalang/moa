all:
	node mc a.js a.moa
	fswatch -0 -o -l 2 ./ | xargs -n1 -I{} -0 node mc a.js a.moa
