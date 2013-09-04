#include <stdio.h>
#include <stdlib.h>

extern int _c0_main();

/* The main function, which calls _c0_main */
int main() {
  printf("%d\n", _c0_main());
  exit(0);
}
