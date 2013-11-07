#include <stdio.h>
#include <stdlib.h>
/* #include <stdbool.h> */
/* using int for bool as of Oct 15, 2013 */

#ifndef _15411_H_
#define _15411_H_

typedef int c0_float;
typedef int c0_bool;

union float_or_int {
  int as_int;
  float as_float;
};

typedef union float_or_int float_or_int;

c0_float fadd(c0_float a, c0_float b);
c0_float fsub(c0_float a, c0_float b);
c0_float fmul(c0_float a, c0_float b);
c0_float fdiv(c0_float a, c0_float b);
c0_bool fless(c0_float a, c0_float b);
c0_float itof(int a);
int ftoi(c0_float a);
int print_fpt(c0_float a);
int print_int(int n);
int print_hex(int n);

#endif
