#include <stdio.h>
#include "cyc.h"

/* Initialize the cycle counter */
static unsigned cyc_hi = 0;
static unsigned cyc_lo = 0;

/* Set *hi and *lo to the high and low order bits  of the cycle counter.
   Implementation requires assembly code to use the rdtsc instruction. */
void access_counter(unsigned *hi, unsigned *lo)
{
	asm __volatile__ ("xorl %%eax, %%eax; cpuid"
	        ::: "%rax", "%rbx", "%rcx", "%rdx");
	asm __volatile__ ("rdtsc"   /* Read cycle counter */
	        : "=a" (*lo), "=d" (*hi));
}

/* Record the current value of the cycle counter. */
void start_counter()
{
	access_counter(&cyc_hi, &cyc_lo);
}

unsigned long to64(unsigned hi32, unsigned lo32) {
	return (((unsigned long)hi32)<<32) + (unsigned long)lo32;
}

/* Return the number of cycles since the last call to start_counter. */
unsigned long get_counter()
{
	unsigned ncyc_hi, ncyc_lo;
	unsigned long now, before;

	/* Get cycle counter */
	access_counter(&ncyc_hi, &ncyc_lo);
	now = to64(ncyc_hi, ncyc_lo);
	before = to64(cyc_hi, cyc_lo);
	return (now - before);
}
