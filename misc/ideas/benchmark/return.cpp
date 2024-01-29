#include <iostream>
#include <unistd.h>
#include <signal.h>

volatile int flag = 1;

int g() {
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
    i++;
  }
  std::cout << i << std::endl;
  return 0;
}
