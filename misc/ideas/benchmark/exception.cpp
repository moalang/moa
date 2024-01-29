#include <iostream>
#include <unistd.h>
#include <signal.h>

volatile int flag = 1;

class Error {};

int g() {
  throw Error();
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
    try {
      f();
    } catch(Error& e) {
      i++;
    }
  }
  std::cout << i << std::endl;
  return 0;
}
