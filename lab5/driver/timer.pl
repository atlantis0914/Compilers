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
our $Opt_Color          = "auto"; # -c, color
our $Opt_Help           = 0;    # -h, help
our $Opt_Make           = 1;    # -m, build compiler (on by default)
our $Opt_Quiet          = 0;    # -q, quiet
our $Opt_Verbose        = 0;    # -v (stacks)
our @Testfiles;                 # files... (default ../tests0)

my $num_tried0      = 0;    # number of tests0 tried
my $num_succeeded0  = 0;    # number of tests0 succeeded
my $score0          = 0;    # score for suite 0
my $num_tried1      = 0;    # number of tests1 tried
my $num_succeeded1  = 0;    # number of tests1 succeeded
my $score1          = 0;    # score for suite 1
my $result          = "";   # result string for Autolab server

Getopt::Long::Configure ("bundling");
my $success = GetOptions (
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
# Do tests grading
#
sub grade_tests {
    my $tried = 0;          # number of tests tried
    my @failed = ();
    my $succeeded = 0;      # number of tests succeeded
    my $score = 0;              # total score

    my $grading = 0;

    printf("-- Validating test files --\n");

    if (!@Testfiles) {
        $grading = 1;
        @Testfiles = glob "$TEST_SUITES_PATH/tests/*.l$LAB";
    }

    foreach my $test (@Testfiles) {
        $tried++;
        if (validate($test)) {
            $succeeded++;
        } else {
            push @failed, $test;
        }
    }

    printf("\n-- Summary --\n");

    if (@failed) {
        printf("Tests failed:\n");
        print map {"   $_\n"} @failed;
    }

    if (!$grading) {
        printf("Validated $succeeded / $tried specified tests\n");
        return;
    } else {
        my ($grade, $max) = tests_grade($tried, $succeeded);
        printf("Validated $succeeded / $tried tests for a score of $grade / $max points\n");
        my $result = join(':', $tried, $succeeded, $grade);
        report_result($result, 0);
    }
}

###
# grade student compiler
#
sub grade_compiler {
    my $tried = 0;
    my @failed = ();
    my $succeeded = 0;
    my $score = 0;

    my @failedhere;

    if ($Opt_Make) {
        printf("-- Building compiler --\n");
        make_compiler();      # aborts if unsuccessful
    }

    printf("-- Testing compiler --\n");

    # individual tests
    if (!@Testfiles) {
		@Testfiles = glob "$TEST_SUITES_PATH/$BENCH_SUITE/*.$BENCH_LEXT";
	}

	printf("-- Running benchmark --\n");
	run_bench(@Testfiles);
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
	my %sums;
	foreach $flag (@BENCH_FLAGS) {
		foreach $safe (@BENCH_SAFES) {
			my $key = "$safe $flag";
			$sums{$key} = 0;
		}
	}
    my @failed = ();
    foreach my $test (@_) {
		my ($g, $c) = bench($test, \%sums);
		$good += $g;
		$count += $c;
    }
	print " --- Summary ---\n";
	printf("Total test runs:      %4d\n", $count);
	printf("Tests which improved: %4d\n", $good);
	print "\n";
	my $base;
	foreach $flag (@BENCH_FLAGS) {
		foreach $safe (@BENCH_SAFES) {
			my $key = "$safe $flag";
			if ( $flag eq $BENCH_FLAGS[0] && $safe eq $BENCH_SAFES[0] ) {
				$base = $sums{$key};
				printf("Total for %12s: %4d\n", $key, $sums{$key});
			} else {
				my $ratio = $sums{$key} / $base;
				my $s = sprintf("Total for %12s: %4d (%1.4f)\n", $key, $sums{$key}, $ratio);
				color_range($s, $ratio);
			}
		}
	}
}


###
# $pass = test (<f>.$LEXT)
# test compiler on given file
#
sub bench {
    my $file = shift;
	my $sums = shift;
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

    printq(2, "-- Testing file $file --\n", $write);

    ($directive, $expected) = read_testdirective($file);

	my $first = 1;
	my $base;
	my $flag;
	my $safe;
	my $good = 0;
	my $count = 0;

	foreach $flag (@BENCH_FLAGS) {
		foreach $safe (@BENCH_SAFES) {
			my $args = "$COMPILER_ARGS $safe $flag";
			my $key = "$safe $flag";

			$asm_file = asm_suffix($file);
			if (-e $asm_file) {move($asm_file, $asm_file.".old")
					or die "could not rename $asm_file from previous compilation\n";}
			$command = "$COMPILER_EXEC $args $file";
			($result, $timeout) = system_with_timeout($COMPILER_TIMEOUT, "$command",
													  $write, $write);

			if ($result == 0) {
				$command = "$GCC $asm_file $BENCH_LINK";
				($result, $timeout) = system_with_timeout($GCC_TIMEOUT, $command,
														  $write, $write);
			} else {
				die "compilation timed out";
			}

			$command = "./bench -d 0";
			my $in_file = in_suffix($file);
			if (-f $in_file) {
				$in_file = "< $in_file";
			} else {
				$in_file = undef;
			}
			($result, $timeout) = system_with_timeout($BENCH_RUN_TIMEOUT, $command,
													  "> a.result", "> /dev/null",
													  $in_file);
			my $cycles = read_file("a.result");
			chomp $cycles;

			print pack("A15", "$key: ");
			$sums->{$key} = $sums->{$key} + $cycles;
			if ($first) {
				$base = $cycles;
				printf("%15d\n", $cycles);
				$first = 0;
			} else {
				my $ratio = $cycles / $base;
				my $s = sprintf("%15d (%1.4f)\n", $cycles, $ratio);
				$count++;

				$good += color_range($s, $ratio);
			}
		}
	}
	print "\n";
	breakout:

	return ($good, $count)
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

sub color_range {
	my ($string, $val) = @_;
	my $good = 0;
	if ($val > $BENCH_OUTLYING) {
		printq(0, color('bold red')) if $Opt_Color;
	} elsif ($val > 1 + $BENCH_EPSILON) {
		printq(0, color('red')) if $Opt_Color;
	} elsif ($val > 1 - $BENCH_EPSILON) {
		printq(0, color('blue')) if $Opt_Color;
	} elsif ($val > (1 / $BENCH_OUTLYING)) {
		printq(0, color('green')) if $Opt_Color;
		$good = 1;
	} else  {
		printq(0, color('bold green')) if $Opt_Color;
		$good = 1;
	}
	printq(0, $string);
	printq(0, color('reset')) if $Opt_Color;
	return $good;
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
