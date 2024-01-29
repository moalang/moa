all:
	@g++ -O2 -std=c++20 return.cpp    && ./a.out > /dev/null && echo "return\t$$(./a.out)"
	@g++ -O2 -std=c++20 global.cpp    && ./a.out > /dev/null && echo "global\t$$(./a.out)"
	@g++ -O2 -std=c++20 tuple.cpp     && ./a.out > /dev/null && echo "tuple\t$$(./a.out)"
	@g++ -O2 -std=c++20 jump.cpp      && ./a.out > /dev/null && echo "jump\t$$(./a.out)"
	@g++ -O2 -std=c++20 exception.cpp && ./a.out > /dev/null && echo "c++throw\t$$(./a.out)"
