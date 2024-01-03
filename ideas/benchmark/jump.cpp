#include <iostream>
#include <setjmp.h>
#include <unistd.h>
#include <signal.h>

volatile int flag = 1;
jmp_buf jump_buffer;

int g() {
  longjmp(jump_buffer, 1);
  return 1;
}

int f() {
  return g();
}

void shutdown(int signum) {
  flag = 0;
}

int main() {
  signal(SIGALRM, shutdown);
  alarm(1);
  long long i = 0;
  while (flag) {
    if (setjmp(jump_buffer) == 0) {
      f();
    } else {
      i++;
    }
  }
  std::cout << i << std::endl;
  return 0;
}
