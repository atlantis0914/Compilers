/*******************************************************************
 *
 * bench.c - Benchmark run for 15-411 Compiler Design Lab 6 optimization
 *
 ********************************************************************/

#define _GNU_SOURCE
#include <sched.h>
#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <getopt.h>
#include <time.h>

#include "cyc.h"
#include "15411.h"

#define MAXITERS 1000		/* Max number of iterations of benchmarks */
#define KBEST 12
#define EPSILON 0.01		/* KBEST samples should be within EPSILON */

/* extern int _c0_main(); */    /* unused here */
extern void* _c0_init(int param);
extern void _c0_prepare(void* data, int param);
extern void _c0_run(void* data, int param);
extern void _c0_checksum(void* data, int param);

/* Other globals */
static int debug_level = 0;

static unsigned long values[KBEST];
static int samplecount = 0;

static int add_sample(unsigned long val) {
  int pos = 0;
  if (samplecount < KBEST) {
    pos = samplecount;
    values[pos] = val;
  } else if (val < values[KBEST-1]) {
    pos = KBEST-1;
    values[pos] = val;
  }
  samplecount++;
  /* Insertion sort */
  while (pos > 0 && values[pos-1] > values[pos]) {
    unsigned long temp = values[pos-1];
    values[pos-1] = values[pos];
    values[pos] = temp;
    pos--;
  }

  return (samplecount >= KBEST
	  && (1+EPSILON)*(double)values[0] >= (double)values[KBEST-1]);
}

/* Function prototypes */
void usage(char *progname);
void init_timeout(int timeout);
unsigned long time_run();

/*
 * Main routine
 */
int main(int argc, char *argv[]) {
  char c;
  int i;
  void* data;
  int param;
  unsigned long cycles;
  /* removing scheduling affinity, -fp 2 Nov 2013 */
  /*
    cpu_set_t s;
  */
  /* Initialization */
  
  debug_level = 0;
  param = 1000;

  /* parse command line args */
  while ((c = getopt(argc, argv, "hd:n:")) != -1) {
    switch (c) {
    case 'd': /* debug level */
      debug_level = atoi(optarg);
      break;

    case 'h': /* print help message */
      usage(argv[0]);
      break;

    case 'n': /* parameter value */
      param = atoi(optarg);
      break;

    default: /* unrecognized argument */
      usage(argv[0]);
    }
  }

#ifdef __linux__
  cpu_set_t s;
  CPU_ZERO(&s);
  CPU_SET(0, &s);

  if (sched_setaffinity(0, sizeof(s), &s) < 0) {
    perror("sched_setaffinity");
    exit(-1);
  }
#endif

  /* Initialize and touch the input data */
  samplecount = 0;

  /* One run to warm up cache */
  data = _c0_init(param);
  _c0_prepare(data, param);
  time_run(data, param);
  /* Timing runs */
  for (i = 0; i < MAXITERS; i++) {
    _c0_prepare(data, param);
    cycles = time_run(data, param);
    if (add_sample(cycles)) {
      i++; break;
    }
  }

  cycles = 0;
  for (i = 0; i < KBEST; i++) {
    cycles += values[i];
  }
  cycles /= KBEST;

  // cycles = values[0];

  /* Print best score */
  if (debug_level >= 1) fprintf(stderr,"Iterations: %d\n", i);
  if (debug_level >= 1) fprintf(stderr, "Best cycles: %lu\n", values[0]);
  if (debug_level >= 1) fprintf(stderr, "%d-Best cycles: %lu\n", KBEST, cycles);
  printf("%lu\n", cycles);
  exit(0);
}

/*
 * time_run - Time a benchmark
 */
unsigned long time_run(void* data, int param) {
  unsigned long cycles;
  double secs;

  /* Time the function */
  /* no input, so no need to rewind */
  /* rewind(stdin); */
  start_counter();
  _c0_run(data, param);
  cycles = get_counter();
  if (debug_level >= 2) fprintf(stderr, "%lu cycles\n", cycles);
  return cycles;
}

/*
 * usage - print a usage message
 */
void usage(char *progname) {
	fprintf(stderr, "usage: %s [-hg]\n", progname);
	fprintf(stderr, "  -h        Print this message\n");
	fprintf(stderr, "  -d <D>    Set debug level (default = 0)\n");
        fprintf(stderr, "  -n <N>    Set <N> as benchmark parameter (default = 1000)\n");
	exit(0);
}
