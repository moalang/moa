compile:
	@clang -S -emit-llvm -O3 a.c
	@cat a.ll
	@echo "\n\n"
	@lli a.ll
