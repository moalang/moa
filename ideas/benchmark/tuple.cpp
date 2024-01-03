#include <iostream>
#include <time.h>

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

int main() {
  auto t1 = clock();
  int i = 0;
  while (true) {
    i++;
    Error<int> e = f();
    if (e.failed) {
      if ((double)(clock() - t1) / CLOCKS_PER_SEC >= 1) {
        break;
      }
    }
  }
  std::cout << i << std::endl;
  return 0;
}
