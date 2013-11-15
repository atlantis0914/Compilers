#!/usr/bin/perl
##############################################################
## driver.pl - 15-411 Driver                                ##
##                                                          ##
## Driver for CMU, 15-411 Compiler Design, Fall 2007-2011   ##
## Functionality:  building compiler, compiling tests,      ##
##                 validating tests with reference compiler ##
##                 testing compiler                         ##
##                                                          ##
##############################################################

use strict;
use warnings;

use lib ".";
use Driverlib;
use DriverConfig;

use File::Copy;
use Getopt::Long;
use Pod::Usage;

use Term::ANSIColor;

############################################################

###
# Declarations and Initialization
#
our $Opt_Autograding    = 0;    # -A, autograding
our $Opt_Color          = "auto"; # -c, color
our $Opt_Help           = 0;    # -h, help
our $Opt_Make           = 1;    # -m, build compiler (on by default)
our $Opt_Quiet          = 0;    # -q, quiet
our $Opt_Verbose        = 0;    # -v (stacks)
our @Testfiles;                 # files... (default ../tests0)

# my $num_tried0      = 0;    # number of tests0 tried
#  my $num_succeeded0  = 0;    # number of tests0 succeeded
# my $score0          = 0;    # score for suite 0
# my $num_tried1      = 0;    # number of tests1 tried
# my $num_succeeded1  = 0;    # number of tests1 succeeded
# my $score1          = 0;    # score for suite 1
my $result          = "";   # result string for Autolab server

Getopt::Long::Configure ("bundling");
my $success = GetOptions (
    'A|autograde'   => \$Opt_Autograding,
    'c|color=s'     => \$Opt_Color,
    'h|help'        => \$Opt_Help,
    'make!'         => \$Opt_Make,
    'q|quiet+'      => \$Opt_Quiet,
    'v|verbose+'    => \$Opt_Verbose,
);
$success or pod2usage();

pod2usage(1) if $Opt_Help;

@Testfiles = @ARGV;
$DEBUG = $Opt_Verbose;

$Opt_Quiet = 2 if 0;
$QUIET = $Opt_Quiet;

if ($Opt_Color eq "auto") {
	$Opt_Color = -t STDOUT;
}
elsif ($Opt_Color eq "on") {
	$Opt_Color = 1;
}
else {
	$Opt_Color = 0;
}

main();

############################################################

###
# Main program
#
sub main {
    autoflush STDOUT 1;

	grade_compiler();

    exit(0);
}

###
# grade student compiler
#
sub grade_compiler {
    # my $tried = 0;
    # my @failed = ();
    # my $succeeded = 0;
    # my $score = 0;
    my %results;
    my $result = '';

    my @failedhere;

    if ($Opt_Make) {
        printf("-- Building compiler --\n");
        make_compiler();      # aborts if unsuccessful
    }

    printf("-- Benchmarking compiler --\n");

    # individual tests
    if (!@Testfiles) {
      @Testfiles = glob "$TEST_SUITES_PATH/$BENCH_SUITE\{0,1\}/*.$BENCH_LEXT";
    }

    printf("-- Running benchmark --\n");
    %results = run_bench(@Testfiles);
    $result = '0';
    for my $file (@Testfiles) {
      if (defined($results{$file})) {
        $result = join(':', $result, $results{$file});
      } else {
        $result = join(':', $result, '0');
      }
    }
    report_result($result, $Opt_Autograding);
  }

###
# Grade one test suite
# ($tried, $succeeded) = run_suite(file1, file2, ...);
#
sub run_bench {
  my $good = 0;
  my $count = 0;
  my $flag;
  my $safe;
  my %results;
  my @failed = ();
  foreach my $test (@_) {
    bench($test, \%results);
  }
  return %results;
}

###
# $pass = test (<f>.$LEXT)
# test compiler on given file
#
sub bench {
    my $file = shift;
    my $results = shift;
    my ($directive, $expected, $valid, $error, $asm_file, $result, $line);
    my $command;
    my $ret;
    my $timeout;
    if (-e "a.out") { unlink "a.out" or die "could not remove a.out\n"; }
    if (-e "a.result") { unlink "a.result" or die "could not remove a.result\n"; }

    my ($read, $write);
    if ($Opt_Quiet == 0) {
        open($write, ">&STDOUT");
    }
    else {
        pipe($read, $write);
    }
    my $p = [$read, $write];

    printq(2, "-- Timing file $file --\n", $write);

    ($directive, $expected) = read_testdirective($file);

	my $base;
	my $flag;
	my $safe;

	foreach $flag (@BENCH_FLAGS) {
		foreach $safe (@BENCH_SAFES) {
			my $args = "$COMPILER_ARGS $safe $flag";
			my $key = "$safe $flag";

			$asm_file = asm_suffix($file);
			if (-e $asm_file) {move($asm_file, $asm_file.".old")
					or die "could not rename $asm_file from previous compilation\n";}
			$command = "$COMPILER_EXEC $args $file";
			($result, $timeout) = system_with_timeout($COMPILER_TIMEOUT, "$command", $write, $write);

                        if ($result != 0) {
                          printf("compilation failure\n");
                        } else {

                        $command = "$GCC $asm_file $BENCH_LINK";
                        ($result, $timeout) = system_with_timeout($GCC_TIMEOUT, $command, $write, $write);

                        if ($result != 0) {
                          printf("assembly failure\n");
                        } else {

			$command = "./bench -d 0";
			($result, $timeout) = system_with_timeout($BENCH_RUN_TIMEOUT, $command,
                                                                  "> a.result", "> /dev/null"); # ,$in_file
                        if ($result != 0) {
                          print("execution failure\n");
                        } else {

			my $cycles = read_file("a.result");
			chomp $cycles;

			print pack("A15", "$key: ");
                        printf("%15d\n", $cycles);
                        # take the last result always, for now
                        $results->{$file} = $cycles;
                      }}}}
              }
    print "\n";
}

###
# make_compiler
# runs make and aborts if unsuccessful
#
sub make_compiler {
    my $result;
    my $write;
    my $timeout;

    if ($Opt_Quiet < 4) {
        $write = undef;
    }
    else {
        $write = ">/dev/null";
    }

    ($result, $timeout) = system_with_timeout($MAKE_TIMEOUT, "make $COMPILER",
                                              $write, $write);
    if ($result != 0) {
        die "make did not succeed\n";
    }
    if (!-e "$COMPILER_EXEC" || !-x "$COMPILER_EXEC") {
        die "compiler does not exist or is not executable\n";
    }
    return 0;
}

sub suffix {
    my $file = shift;
    my $suffix = shift;
    my @fields = split(/[.]/, $file);
    if (scalar @fields == 1) {
        return $fields[0].".".$suffix; # append ".s" if no extension
    } else {
        @fields[@fields-1]=$suffix; # replace extension with "s" otherwise
        return join('.', @fields);
    }
}

###
# asm_suffix(<file>) = <file>.s
#
sub asm_suffix {
    my $file = shift;
    suffix($file, "s");
}

sub in_suffix {
    my $file = shift;
	return "$file.in"
}

###
# ($directive, $value) = read_testdirective($file)
# parses the first line of a test file
# #test <directive> <value>
# where value is optional
# $directive may be undefined (if line is not well-formed),
# as may be $value
#
sub read_testdirective {
    my $file = shift;
    my $line;
    my ($test, $directive, $value);
    open(MYFILE, "<$file") or die "can't open test file: $file\n";
    $line = <MYFILE>;
    $line =~ s/\s*$//;
    ($test, $directive, $value) = split(/\s+/, $line, 3);
    if ($test eq "//test") {
        if (defined $value) {
            chomp $value;
        }
        return ($directive, $value);
    } else {
        return (undef, undef);
    }
}

###
# Check validity of the test directive
# returns (1, undef) if valid, (0, errormessage) otherwise 
#
sub test_directive_verify {
    my $directive = shift;
    my $expected = shift;

    unless (defined $directive) {
        return (0, "Bad test: test directive not found\n");
    }
    unless ($directive eq "error" || $directive eq "exception"
            || $directive eq "return") {
        return (0, "Bad test: unrecognized test directive $directive\n");
    }
    if ($directive eq "return" && !defined $expected) {
        return (0, "Bad test: return directive with no value\n");
    }

    return (1, undef);
}

__END__

############################################################

=pod

=head1 NAME

driver.pl - 15-411 Autolab/testing driver

=head1 SYNOPSIS

driver.pl [options] [files]

  $ ./driver.pl
  $ ./driver.pl ../test0/return01.l1

=head1 DESCRIPTION

This directory contains the driver files for testing your compiler. These are
identical to the files the Autolab server uses identical files to grade your
code upon hand-in.

If you do not specify any tests, the default behaviour is to run the files in ../bench/

=over 8

=item ../tests/*.<extension>

Run on all relevant tests when doing validation

=back

=head1 OPTIONS

=over 8

=item -c [on|off|auto], --color [on|off|auto]

Make pass/fail messages colorful.

=item -h, --help

Show this help message.

=item --nomake

Do not build the compiler before beginning grading.

=item -q, --quiet

Quiet mode. Pass multiple times for quieter grading:

=over 4

=item -q emits only one line of output for passing tests

=item -qq silences passing tests

=item -qqq silences all tests

=item -qqqq silences building the compiler

=back

=item -v, --verbose

Enable driver debugging output. Pass more times for more output.

=back

=cut
