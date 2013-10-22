#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef int c0_float;

union float_or_int {
  int as_int;
  float as_float;
};

typedef union float_or_int float_or_int;

c0_float fadd(c0_float a, c0_float b) {
  float_or_int x; x.as_int = a;
  float_or_int y; y.as_int = b;
  float_or_int z; z.as_float = x.as_float + y.as_float;
  return z.as_int;
}

c0_float fsub(c0_float a, c0_float b) {
  float_or_int x; x.as_int = a;
  float_or_int y; y.as_int = b;
  float_or_int z; z.as_float = x.as_float - y.as_float;
  return z.as_int;
}

c0_float fmul(c0_float a, c0_float b) {
  float_or_int x; x.as_int = a;
  float_or_int y; y.as_int = b;
  float_or_int z; z.as_float = x.as_float * y.as_float;
  return z.as_int;
}

c0_float fdiv(c0_float a, c0_float b) {
  float_or_int x; x.as_int = a;
  float_or_int y; y.as_int = b;
  float_or_int z; z.as_float = x.as_float / y.as_float;
  return z.as_int;
}

bool fless(c0_float a, c0_float b) {
  float_or_int x; x.as_int = a;
  float_or_int y; y.as_int = b;
  return x.as_float < y.as_float;
}

c0_float itof(int a) {
  float_or_int x; x.as_float = (float)a;
  return x.as_int;
}

int ftoi(c0_float a) {
  float_or_int x; x.as_int = a;
  return (int)x.as_float;
}

int print_fpt(c0_float a) {
  float_or_int x; x.as_int = a;
  fprintf(stderr, "%g\n", x.as_float);
  return 0;
}

int print_int(int n) {
  fprintf(stderr, "%d\n", n);
  return 0;
}

int print_hex(int n) {
  fprintf(stderr, "0x%08X\n", n);
  return 0;
}
