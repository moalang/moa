#include <iostream>
#include <unistd.h>
#include <signal.h>

volatile int flag = 1;
int failed = 0;

int g() {
  failed = 1;
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
    f();
    if (failed) {
      i++;
      failed = 0;
    }
  }
  std::cout << i << std::endl;
  return 0;
}
