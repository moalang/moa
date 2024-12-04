echo -e '#include <stdio.h>\nint main() {\nputs("hello, world"); return 0;\n}' | gcc -x c -o /dev/stdout -pipe -
