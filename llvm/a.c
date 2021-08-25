#include <stdio.h>
#include <stdlib.h>

char at(char *s, int n) {
  return s[n];
}

int main() {
  puts("hi");
  printf("%c\n", at("i", 1));
  return 0;
}
