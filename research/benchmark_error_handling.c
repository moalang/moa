#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <time.h>

typedef struct { int64_t r1; int64_t r2; } r2_t;
typedef struct { int64_t r1; int64_t r2; int64_t r3; } r3_t;
typedef struct { int64_t r1; int64_t r2; int64_t r3; int64_t r4; int64_t r5; int64_t r6; int64_t r7; int64_t r8; int64_t r9; } r9_t;

int global;
int64_t make1() { return global; }
r2_t make2() { r2_t v = {global, 1}; return v; }
r3_t make3() { r3_t v = {global, 1, 1}; return v; }
r9_t make9() { r9_t v = {global, 1, 1, 1, 1, 1, 1, 1, 1}; return v; }

int measure_r1() { return make1() == 0 ? 1 : 2; }
int measure_r2() { return make2().r1 == 0 ? 1 : 2; }
int measure_r3() { return make3().r1 == 0 ? 1 : 2; }
int measure_r9() { return make9().r1 == 0 ? 1 : 2; }

int measure_setjmp() {
  jmp_buf buf;
  setjmp(buf);
  return 1;
}

int measure_longjmp() {
  jmp_buf buf;
  if (setjmp(buf) == global) {
    longjmp(buf, global);
    exit(-1);
  } else {
    return 1;
  }
}

static void benchmark(const char* label, uint64_t n, int(*f)()) {
  uint64_t i;
  clock_t start = clock();
  for (i=0; i < n; ++i) {
    global += f();
  }
  clock_t delta = clock() - start;
  printf("%s: %lums\n", label, delta / 1000);
}

int main() {
  uint64_t count = 10 * 1000 * 1000;
  printf("count: %llu\n", count);
  benchmark("return1", count, measure_r1);
  benchmark("return2", count, measure_r2);
  benchmark("return3", count, measure_r3);
  benchmark("return9", count, measure_r9);
  benchmark("setjmp", count, measure_setjmp);
  benchmark("longjmp", count, measure_longjmp);
}
