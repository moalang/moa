#include <setjmp.h>
#include <time.h>
#include <iostream>

jmp_buf jump_buffer;

int g() {
  longjmp(jump_buffer, 1);
  return 1;
}

int f() {
  return g();
}

int main() {
  auto t1 = clock();
  int i = 0;
  while (true) {
    i++;
    if (setjmp(jump_buffer) == 0) {
      f();
    } else {
      if ((double)(clock() - t1) / CLOCKS_PER_SEC >= 1) {
        break;
      }
    }
  }
  std::cout << i << std::endl;
  return 0;
}
