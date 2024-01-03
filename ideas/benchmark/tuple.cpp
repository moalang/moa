#include <iostream>
#include <unistd.h>
#include <signal.h>

volatile int flag = 1;

template<typename T>
struct Error {
  bool failed;
  T ret;
};

Error<int> g() {
  return {true, 0};
}

Error<int> f() {
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
    Error<int> e = f();
    if (e.failed) {
      i++;
    }
  }
  std::cout << i << std::endl;
  return 0;
}
