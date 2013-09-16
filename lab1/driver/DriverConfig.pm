##############################################################
## DriverConfig.pm - 15-411 Driver Configuration File       ##
##                                                          ##
## Contains all configuration values for the                ##
## driver.                                                  ##
##                                                          ##
##############################################################

use strict;
use warnings;

## how we make these truly global
use vars qw($LAB $COMPILER $COMPILER_EXEC $COMPILER_ARGS @LEXTS $GCC $RUNTIME
            $REF_COMPILER $REF_COMPILER_ARGS $MAKE_TIMEOUT $COMPILER_TIMEOUT
            $GCC_TIMEOUT $RUN_TIMEOUT $TEST_SUITES_PATH $MAX_VALIDATE_SCORE
            $MIN_TESTS &tests_grade $CMPL_GRADE $AUTOGRADE_REF_COMPILER);

our $LAB            = 1;

my $rt_stem = "l${LAB}rt";

our $COMPILER       = "l${LAB}c";                       # name of compiler to generate
our $COMPILER_EXEC  = "bin/$COMPILER";                  # compiler executable
our $COMPILER_ARGS  = "";
our @LEXTS          = reverse map {"l$_"} (1 .. $LAB);  # source filename extensions
our $GCC            = "gcc -m64";     # gcc executable and default flags
our $RUNTIME        = "$rt_stem.c";   # runtime system for linking against asm file

my $c0_level = 6 - $LAB;
our $REF_COMPILER = "/afs/cs.cmu.edu/academic/class/15411-f13/bin/cc0";
our $AUTOGRADE_REF_COMPILER = "../reference/bin/cc0";
# our $REF_COMPILER_ARGS = " -C $c0_level";
# cc0 now uses the file name extension to determine language level
our $REF_COMPILER_ARGS = "";

our $MAKE_TIMEOUT       = 100;  # timeout for making compiler
our $COMPILER_TIMEOUT   = 5;    # timeout for running compiler
our $GCC_TIMEOUT        = 8;    # timeout for GCC on asm file
our $RUN_TIMEOUT        = 5;    # timeout for running compiled executable

# path to directory containing tests/, tests0/, tests1/, tests2/, tests3/
our $TEST_SUITES_PATH = "..";

our $MAX_VALIDATE_SCORE = 20;    # maximal score for test case validation
our $MIN_TESTS          = 10;    # minimum number of tests to submit

my $MAX_SCORE0 = 20;        # maximal score for compiler, test suite 0
my $MAX_SCORE1 = 50;        # maximal score for compiler, test suite 1
my $MAX_SCORE2 = 10;        # maximal score for compiler, test suite 2
my $TESTS1_N = 10;      # first n failing suite 1 tests...
my $TESTS1_PTS = 2;     # ...are worth this many points each
my $TESTS0_MIN = 7;     # number of error cases in tests0
my $TESTS1_MIN = 172;     # number of error cases in tests1
my $TESTS2_MIN = 211;     # number of error cases in tests2

sub tests_grade {
    my $tried = shift;
    my $succeeded = shift;
    $tried = $MIN_TESTS if ($tried < $MIN_TESTS);
    my $grade = POSIX::floor($MAX_VALIDATE_SCORE * $succeeded / $tried);
    return ($grade, $MAX_VALIDATE_SCORE);
}

our $CMPL_GRADE = {
    "tests0" => sub {
        my $tried = shift;
        my $succeeded = shift;
        my $grade;

        if ($succeeded > $TESTS0_MIN) {
            $grade = POSIX::floor($MAX_SCORE0 * ($succeeded - $TESTS0_MIN)
                                              / ($tried - $TESTS0_MIN));
        } else {
            $grade = 0;
        }

        return ($grade, $MAX_SCORE0);
    },
    "tests1" => sub {
        my $tried = shift;
        my $succeeded = shift;
        my $grade;

        # use piecewise linear scoring for test suite 1
        # first $TESTS1_N tests are worth $TESTS1_PTS each
        # linear after that to $TESTS1_MIN
        if ($succeeded >= $tried - $TESTS1_N) {
            $grade = $MAX_SCORE1 - ($tried - $succeeded) * $TESTS1_PTS;
        } elsif ($succeeded >= $TESTS1_MIN) {
            $grade = POSIX::floor(($MAX_SCORE1 - $TESTS1_N * $TESTS1_PTS)
                                  * ($succeeded - $TESTS1_MIN)
                                  / ($tried - $TESTS1_N - $TESTS1_MIN));
        } else {
            $grade = 0;
        }
        return ($grade, $MAX_SCORE1);
    },
    "tests2" => sub {
        my $tried = shift;
        my $succeeded = shift;
        my $grade;

        if ($succeeded > $TESTS2_MIN) {
            $grade = POSIX::floor($MAX_SCORE2 * ($succeeded - $TESTS2_MIN)
                                              / ($tried - $TESTS2_MIN));
        } else {
            $grade = 0;
        }
        return ($grade, $MAX_SCORE2);
    },
    # no score for bonus problems in tests3
    "tests3" => sub {
      my $tried = shift;
      my $succeeded = shift;
      my $grade = 0;
      return ($grade, 0);
    },
};

# End module with a 1 so that loading it returns TRUE
1;
