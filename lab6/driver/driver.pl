#!/usr/bin/perl
##############################################################
## driver.pl - 15-411 Driver                                ##
##                                                          ##
## Driver for CMU, 15-411 Compiler Design, Fall 2007-2012   ##
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
use File::Spec;
use Getopt::Long;
use Pod::Usage;
use Scalar::Util qw(looks_like_number);

use Term::ANSIColor;
use POSIX ":sys_wait_h";

############################################################

###
# Declarations and Initialization
#
our $Opt_Autograding    = 0;    # -A, autograding, hidden flag for autograder
our $Opt_Color          = "auto"; # -c, color
our $Opt_FailFast       = 0;    # -f, --fail-fast
our $Opt_Help           = 0;    # -h, help
our $Opt_Make           = 1;    # -m, build compiler (on by default)
our @Opt_Pass           = ();   # -p <args>, arguments to compiler
our $Opt_Quiet          = 0;    # -q, quiet
our @Opt_Suite          = ();   # -s <dir>,
our $Opt_Tests          = 0;    # -t, validate test files with ref.comp.
our $Opt_Verbose        = 0;    # -v (stacks)
our $Opt_Progress       = 0;    # -p, progress
our $Opt_Parallel       = 1;    # -j, parallel
our $Opt_FailureFile    = "";   # -w, failure file
our $Opt_RerunFile      = "";   # -R, rerun file
our $Opt_RunFile        = "";   # -r, run file
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
    'A|autograde'      => \$Opt_Autograding,
    'c|color=s'        => \$Opt_Color,
    'f|fail-fast!'     => \$Opt_FailFast,
    'h'                => sub { $Opt_Help = 1 },
    'help'             => sub { $Opt_Help = 2 },
    'make!'            => \$Opt_Make,
    'p|pass=s'         => \@Opt_Pass,
    'q|quiet+'         => \$Opt_Quiet,
    's|suite=s@'       => \@Opt_Suite,
    't|tests'          => \$Opt_Tests,
    'v|verbose+'       => \$Opt_Verbose,
    'g|progress'       => \$Opt_Progress,
    'j|parallel=i'     => \$Opt_Parallel,
    'w|failure-file=s' => \$Opt_FailureFile,
    'R|rerun-file=s'   => \$Opt_RerunFile,
    'r|test-file=s'    => \$Opt_RunFile,
);

if (!$success || $Opt_Help == 1) {
  pod2usage(-verbose => 99, -sections => "SYNOPSIS|SUMMARY");
} elsif ($Opt_Help == 2) {
  pod2usage(-verbose => 2) if $Opt_Help;
}

@Testfiles = @ARGV;
$DEBUG = $Opt_Verbose;

$Opt_Quiet = 2 if $Opt_Autograding;
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

$Opt_FailureFile = $Opt_RerunFile if !$Opt_FailureFile;
$Opt_RunFile = $Opt_RerunFile if !$Opt_RunFile;

if (@Opt_Suite && @ARGV) {
    die "Not allowed to specify a suite *and* individual tests!";
}

if ($Opt_RunFile && @Opt_Suite) {
    die "Not allowed to specify a run file *and* a suite!";
}

if ($Opt_RunFile && @ARGV) {
    die "Not allowed to specify a rerun file *and* individual tests!";
}

if ($Opt_Autograding && (@Opt_Suite || @ARGV)) {
    die "Autograding only runs on default suite of tests";
}

foreach my $suite (@Opt_Suite) {
    -d "../$suite" or die "Bad suite: ../$suite not a folder";
}

if ($Opt_RunFile) {
  open (TESTS, $Opt_RunFile);
  while (my $line = <TESTS>) {
    chomp $line;
    push @Testfiles, $line;
  }
  close TESTS;
}

my $ref_compiler = 0;

main();

############################################################

###
# Main program
#
sub main {
    autoflush STDOUT 1;

    if ($Opt_Tests) {
        grade_tests();
    } else {
        grade_compiler();
    }

    exit(0);
}

###
# Do tests grading
#
sub grade_tests {
    my $tried = 0;          # number of tests tried
    my %failed = ();
    my $succeeded = 0;      # number of tests succeeded
    my $score = 0;              # total score

    my $grading = 0;

    printf("-- Validating test files --\n");

    if (!@Testfiles) {
        $grading = 1;
        @Testfiles = glob "$TEST_SUITES_PATH/tests/*.l$LAB";
    }

    $ref_compiler = 1;
    ($tried, $succeeded, %failed) = run_suite(@Testfiles);

    printf("\n-- Summary --\n");

    for my $type (keys %failed) {
        my @failures = @{$failed{$type}};
        if (@failures) {
            print "$type failures:\n";
            print map {"   $_\n"} @failures;
        }
    }

    if (!$grading) {
        printf("Validated $succeeded / $tried specified tests\n");
        return;
    } else {
        my ($grade, $max) = tests_grade($tried, $succeeded);
        printf("Validated $succeeded / $tried tests for a score of $grade / $max points\n");
        my $result = join(':', $tried, $succeeded, $grade);
        report_result($result, $Opt_Autograding);
    }
}

###
# grade student compiler
#
sub grade_compiler {
    my $tried = 0;
    my %failed = ();
    my $succeeded = 0;
    my $score = 0;
    my %results = ();

    if ($Opt_Make) {
        printf("-- Building compiler --\n");
        make_compiler();      # aborts if unsuccessful
    }

    printf("-- Testing compiler --\n");

    if (@Testfiles) {
        # individual tests
        ($tried, $succeeded, %failed) = run_suite(@Testfiles);
    } else {
        # full test suites
        if (!@Opt_Suite) {
            @Opt_Suite = sort keys %$CMPL_GRADE;
            # don't want to grade "tests"
            # push @Opt_Suite, "tests";
        }

        foreach my $suite (@Opt_Suite) {
            printf("-- Running $suite --\n");
            my @files;
            my %failedhere = ();
            foreach my $lext (@LEXTS) {
                push @files, glob "$TEST_SUITES_PATH/$suite/*.$lext";
            }
            ($tried, $succeeded, %failedhere) = run_suite(@files);
            my $done = 0;
            for my $type ( keys %failedhere ) {
              if (@{$failedhere{$type}}) {
                $done = $Opt_FailFast;
              }
              push @{$failed{$type}}, @{$failedhere{$type}}
            }
            if ($done) { last; }
            $results{$suite} = [$tried, $succeeded];
        }
    }

    if ($Opt_FailFast && %failed) {
        printf("Not running remaining tests.\n");
        return;
    }

    printf("\n-- Summary --\n");

    for my $type (keys %failed) {
        my @failures = @{$failed{$type}};
        if (@failures) {
            print "$type failures:\n";
            print map {"   $_\n"} @failures;
        }
    }
    record_failures(\%failed);

    if (@Opt_Suite) {
        my $total = 0;
        my $maxtotal = 0;
        foreach my $suite (sort keys %results) {
            my ($tried, $succeeded) = @{$results{$suite}};
            if ($CMPL_GRADE->{$suite}) {
                my ($grade, $max) = &{$CMPL_GRADE->{$suite}}($tried, $succeeded);
                $total += $grade;
                $maxtotal += $max;
                printf("$suite: passed $succeeded / $tried tests for a score of $grade / $max points\n");
                $results{$suite} = [$tried, $succeeded, $grade];
            }
            else {
                printf("$suite: passed $succeeded / $tried tests on unknown suite\n");
            }
        }
        printf("Total points on tested suites: $total / $maxtotal\n");

        if ($results{tests0} && $results{tests1} && $results{tests2}) {
            my $result = join(':', @{$results{tests0}}, @{$results{tests1}},
                                   @{$results{tests2}}, @{$results{tests3}});
            report_result($result, $Opt_Autograding);
        }
    }
}

##
# Record the failures if necessary
#
sub record_failures {
  # woo I know how to write good perl!
  my $_failures = shift;
  my %failures = %{${_failures}};
  if (!$Opt_FailureFile) { return; }
  open(FAILURES, ">$Opt_FailureFile") or die "Couldn't open $Opt_FailureFile";
  for my $type (keys %failures) {
    for my $file ( @{$failures{$type}} ) {
      print FAILURES "$file\n";
    }
  }
  close FAILURES;
}

###
# Grade one test suite
# ($tried, $succeeded) = run_suite(file1, file2, ...);
#
sub run_suite {
    my $tried = 0;
    my $succeeded =0;
    my %failed = ( 'error' => [], 'return' => [], 'exception' => [] );
    my $total = @_;
    my @workers = ();

    ##
    # Add some work to the queue (forking off and performing it
    #   $addwork->($test, $cmd, $timeout, $out, $err, $input, $cont, $type)
    #
    my $addwork = sub {
      my ($test, $cmd, $timeout, $out, $err, $input, $cont, $type) = @_;
      my ($read, $write);
      pipe($read, $write);

      my $pid = fork();
      if ($pid == 0) {
        my $ret = system_with_timeout($timeout, $cmd, $out, $err, $input);
        print $write $ret;
        exit 0;
      }
      close $write;

      push @workers, [$pid, $cont, $test, $read, $type];
    };

    ##
    # Wait for some work to be done, processing it after it's done. This can
    # result in more work being generated possibly. Returns whether work should
    # continue or work should cease.
    #   $work->()
    #
    my $work = sub {
        # All workers exit with 0, the work's exit status is written to a pipe
        my $pid = wait;
        if (!WIFEXITED($?) || WEXITSTATUS($?) != 0) { die "Worker died badly"; }
        my $idx;
        for ($idx = 0; $idx < scalar(@workers); $idx++) {
          my $el = $workers[$idx];
          last if @$el[0] == $pid;
        }
        if ($idx == scalar(@workers)) {
          die "Reaped child that wasn't track of";
        }
        my $arr = splice @workers, $idx, 1;

        # Grab the work's result and invoke the continuation with it
        my ($_, $c, $test, $read, $type) = @$arr;
        my $status = <$read>;
        close $read;
        my ($cmd, @rest) = $c->($status);
        if ($#rest > 0) {
          $addwork->($test, $cmd, @rest, $type);
          return 1;
        }

        # If there is no more work for this test, then we can print a summary of
        # what happened to it
        my $worked = $cmd;
        if ($Opt_Progress) {
            progress($tried, $total);
        }
        if ($worked) {
            $succeeded++;
        } else {
            push @{$failed{$type}}, $test;
            if ($Opt_FailFast) {
                return 0;
            }
        }
        return 1;
    };

    my $continue = 1;
    for my $test (@_) {
        # Throttle the number of parallel processes running
        $continue = $work->() while ($continue &&
                                     scalar(@workers) >= $Opt_Parallel);
        last if !$continue;
        $tried++;
        $addwork->($test, test($test));
    }
    if (scalar(@workers) > 0) {
      if (!$continue) {
        printq(3, "Waiting for other processes to finish...\n");
      }
      $work->() while (scalar(@workers) > 0);
    }

    if ($Opt_Progress) {
        print "\n";
    }
    return ($tried, $succeeded, %failed);
}

###
# $pass = test (<f>.$LEXT)
# test compiler on given file. All outputs are scoped to the name of the given
# file but with 'log' prepended to the path components.
#
sub test {
    my $file = shift;

    # Munge on the path names and sanity check the state of the world
    my $log_file = $file;
    $log_file =~ s/(tests\d?)/log\/$1/g;
    my $exe = $log_file;
    my $output = $log_file;
    $exe =~ s/\.(l\d)$/-$1/g;
    $output =~ s/$/.result/g;
    if (-e $exe) { unlink $exe or die "could not remove $exe\n"; }
    if (-e $output) { unlink $output or die "could not remove $output\n"; }
    system("mkdir -p `dirname $log_file`");

    my ($read, $write);
    if ($Opt_Quiet == 0) {
        open($write, ">&STDOUT");
    } else {
        pipe($read, $write);
    }
    my $p = [$read, $write];

    printq(2, "-- Testing file $file --\n", $write);

    my ($directive, $expected) = read_testdirective($file);
    my ($valid, $error) = test_directive_verify($directive, $expected);
    if (!$valid) {
        return fail($error, $file, $p);
    }

    my $result;
    my $command;
    my $callback = sub {
        my $result = shift;
        my $ret = grade_compilation($command, $file, $result, $directive,
                                    $expected, $ref_compiler, $p);
        if (defined $ret) {
            return $ret;
        }

        my $in_file = in_suffix($file);
        if (-f $in_file) {
            $in_file = "< $in_file";
        } else {
            $in_file = undef;
        }
        my $other = $log_file;
        $other =~ s/$/.output/g;

        # If $exe is just 'foo', then the file 'foo' in the current directory
        # won't be executed, but rather it's looked up in $PATH. Hence we take
        # the relative path and make it absolute here.
        my $real_exe = File::Spec->rel2abs($exe);
        return ($real_exe, $RUN_TIMEOUT, "> $output", "> $other", $in_file, sub {
          my $result = shift;
          return grade_execution($file, $result, $directive, $expected, $output,
                                 $other, $p);
        }, $directive);
    };
    if ($ref_compiler) {
        my $compiler = $REF_COMPILER;
        if ($Opt_Autograding) {
          $compiler = $AUTOGRADE_REF_COMPILER;
        }
        $command = "$compiler $REF_COMPILER_ARGS $file -o $exe";
        return ($command, $COMPILER_TIMEOUT, $write, $write, undef, $callback,
                $directive);
    } else {
        my $asm_file = asm_suffix($file);
        $command = "$COMPILER_EXEC $COMPILER_ARGS $file @Opt_Pass";

        return ($command, $COMPILER_TIMEOUT, $write, $write, undef, sub {
            my $result = shift;
            my $asm_dest = $exe . ".s";
            if (-e $asm_dest) {
                move($asm_dest, $asm_dest.".old")
                  or die "could not move $asm_dest from previous compilation\n";
            }
            if ($result != 0) {
              return $callback->($result);
            }

            move($asm_file, $asm_dest) or die "couldn't move $asm_file\n";
            $command = "$GCC $asm_dest $RUNTIME -o $exe";
            return ($command, $GCC_TIMEOUT, $write, $write, undef, $callback);
        }, $directive);
    }
}

###
# $ret = grade_compilation($file, $result, $directive, $expected, $is_ref);
# if (defined $ret) {
#     return $ret;
# }
#
sub grade_compilation {
    my $cmd = shift;
    my $file = shift;
    my $result = shift;
    my $directive = shift;
    my $expected = shift;
    my $is_ref_compiler = shift;
    my $p = shift;

    my $res_known = 1;
    my $res_quarantine = 2;
    my $res_crash = 3;

    if (WIFSIGNALED($result)) {
      return fail("Failed cmd: $cmd\n", $file, $p);
    }
    $result = WEXITSTATUS($result);

    if ($is_ref_compiler && $result == $res_crash) {
        return fail("Compilation crashed: reference compiler bug!\n"
                   ."Send mail to the course staff, please.\n", $file, $p);
    }

    if ($directive eq "error") {
        if ($result == 0) {
            return fail("Compilation unexpectedly succeeded on $file.\n",
                        $file, $p);
        }
        if (!$is_ref_compiler || $result == $res_known) {
            return pass("Compilation failed on $file as expected.\n",
                        $file, $p);
        }
        if ($result == $res_quarantine) {
            return fail("Future language features used in $file.\n", $file, $p);
        }
        return fail("Mysterious error $result.\n"
                   ."Send mail to course staff, please.\n", $file, $p);
    }

    if ($result != 0) {
        ## build error, but we didn't have an error directive.
        ## bail now.
        if (!$is_ref_compiler || $result == $res_known) {
            return fail("Compilation unexpectedly failed on $file.\n",
                        $file, $p);
        }
        if ($result == $res_quarantine) {
            return fail("Future language features used in $file.\n",
                        $file, $p);
        }
        return fail("Mysterious error $result.\n"
                   ."Send mail to course staff, please.\n", $file, $p);
    }

    return undef;
}

###
# return grade_execution($file, $result, $directive, $expected, $result_file,
#                        $output_file, $pipes);
#
sub grade_execution {
    my $file = shift;
    my $result = shift;
    my $directive = shift;
    my $expected = shift;
    my $result_file = shift;
    my $output_file = shift;
    my $p = shift;

    if ($directive eq "exception") {
        if (WIFEXITED($result)) {
            return fail("Execution of binary unexpectedly succeeded.\n",
                        $file, $p);
        }
        if (!WIFSIGNALED($result)) { die "WSTOPSIG?!"; }
        $result = WTERMSIG($result);
        if ($result == 10) { $result = 11; }
        if (!defined $expected) {
            return pass("Execution of binary raised exception $result.\n",
                        $file, $p);
        }
        $expected = $expected+0;  # convert to integer, for sanity's sake
        if ($result == $expected) {
            return pass("Execution of binary raised appropriate exception $result.\n",
                        $file, $p);
        }
        return fail("Execution of binary raised inappropriate exception $result; expected $expected.\n",
                    $file, $p);
    }
    elsif ($directive eq "return") {
        if (WIFSIGNALED($result)) {
            $result = WTERMSIG($result);
            return fail("Execution of binary unexpectedly failed with exception $result.\n",
                        $file, $p);
        }
        $result = WEXITSTATUS($result);
        # if a.result does not exist, $all_of_file = ''
        my $return = read_file($result_file);
        chomp $return;
        if ($return eq $expected) {
            my $out_file = out_suffix($file);
            if (! -e $out_file) {
                return pass("Correct result\n", $file, $p);
            }
            my $output = read_file($output_file);
            my $expected_output = read_file(out_suffix($file));
            if ($output eq $expected_output) {
                return pass("Correct result\n", $file, $p);
            }
            else {
                return fail("Correct return value, but output differs from expected output\n",
                            $file, $p);
            }
        }
        return fail("Result '$return' differs from expected answer '$expected'.\n",
                    $file, $p);
    }

    print "I can't be here (test $file)! Inconceivable!\n";
    print "Send mail to course staff, please.\n";
    return fail("", $file, $p);
}

###
# make_compiler
# runs make and aborts if unsuccessful
#
sub make_compiler {
    my $result;
    my $write;

    if ($Opt_Quiet < 4) {
        $write = undef;
    }
    else {
        $write = ">/dev/null";
    }

    $result = system_with_timeout($MAKE_TIMEOUT, "make $COMPILER",
                                  $write, $write);
    if ($result != 0) {
        die "make did not succeed\n";
    }
    if (!-e "$COMPILER_EXEC" || !-x "$COMPILER_EXEC") {
        die "compiler does not exist or is not executable\n";
    }
    return 0;
}

###
# Convenience function for passing results
#
sub pass {
    my $msg = shift;
    my $file = shift;
    my $p = shift;

    my ($r, $w) = @$p;
    close($w);
    if (defined $r) {
        close($r);
    }

    printq(0, $msg);

    printq(1, color("green")) if $Opt_Color;
    printq(1, "-- PASS $file --\n");
    printq(1, color("reset")) if $Opt_Color;

    return 1;
}

###
# Convenience function for failing results
#
sub fail {
    my $msg = shift;
    my $file = shift;
    my $p = shift;

    my ($r, $w) = @$p;
    close($w);
    if (defined $r) {
        while (my $line = <$r>) {
            printq(5, $line);
        }
        close($r);
    }

    printq(2, $msg);

    printq(2, color("red")) if $Opt_Color;
    printq(2, "-- FAIL $file --\n");
    printq(2, color("reset")) if $Opt_Color;
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

sub out_suffix {
    my $file = shift;
  return "$file.out"
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
    if (defined $expected && not looks_like_number($expected)) {
        return (0, "Bad test: invalid expected result '$expected', must be numeric\n");
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
  $ ./driver.pl --suite tests0 --suite tests1
  $ ./driver.pl ../test0/return01.l1
  $ ./driver.pl --tests

=head1 DESCRIPTION

This directory contains the driver files for testing your compiler. These are
identical to the files the Autolab server uses to grade your code upon hand-in.

If you do not specify any tests, the default behaviour is:

=over 8

=item -s tests0 -s tests1 -s tests2

Use the full grading suite when testing the compiler

=item ../tests/*.<extension>

Run on all relevant tests when doing validation

=back

=head1 SUMMARY

=head2 Running tests

 -f --fail-fast       Stop running tests when one fails
 -c --color           Specify colors [on|off|auto]
 -p --pass            Specify arguments to pass to the compiler
 -g --progress        Show a progress bar of tests to run
 -j --parallel        Specify number of tasks to run in parallel
 -t --tests           Validate tests in the "tests" directory

 -w --failure-file    File to write failing test cases to
 -r --test-file       File to read test cases from
 -R --rerun-file      Rerun tests specified in this file

=head2 Driver options

 -h                   This help message
 --help               Man page style help
 --nomake             Don't run 'make' before running tests
 -q --quiet           Suppress output from the driver
 -v --verbose         Enable debug output of the driver

=head1 OPTIONS

=over 8

=item -A, --autograde

Emit grading output for Autolab. Implies -qq, since Autolab will keel over and
die if the grader produces too much output.

=item -c [on|off|auto], --color [on|off|auto]

Make pass/fail messages colorful.

=item -f, --fail-fast

When one test fails, cease running tests.

=item -h, --help

Show this help message.

=item --nomake

Do not build the compiler before beginning grading.

=item -p <args>, --pass <args>

Pass arguments along to the compiler.

=item -q, --quiet

Quiet mode. Pass multiple times for quieter grading:

=over 4

=item -q emits only one line of output for passing tests

=item -qq silences passing tests

=item -qqq silences all tests

=item -qqqq silences building the compiler

=back

=item -g, --progress

Output a progress bar of percentage of tests run of total tests left to run.

=item -j <n>, --parallel <n>

Run up to n process simultaneously. Beware, may lead to false negatives if a
test fails due to a timeout because it could be in contention with other tests
running concurrently.

=item -s <suite>, --suite <suite>

Run compiler with test suite <suite>. This option can be passed more than once
to run multiple test suites. This option will be ignored when validating tests.

=item -t, --tests

Validate test files in "tests" suite using reference compiler.

=item -v, --verbose

Enable driver debugging output. Pass more times for more output.

=item -w <file>, --failure-file <file>

Once tests have finished running, all failed tests are written to this file. The
file is overwritten and if there are no failures this will be an empty file.

=item -r <file>, --tests-file <file>

Run all tests specified in the given file. Each line is interpreted as a path to
a test.

=item -R <file>, --rerun-file <file>

Re-runs the tests specified in this file. The file is rewritten after tests have
finished running with all failing tests. This can be used between invocations of
the driver as tests are fixed and the set of failing tests is whittled down.

=back

=cut
