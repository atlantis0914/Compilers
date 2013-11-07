###############################################################################
# 15-411 Driverlib.pm
# A package of helper functions for the 15-411 Autolab driver
#
# Based on the original Driverlib.pm Copyright (c) 2005 David R. O'Hallaron.
###############################################################################

use strict;
use warnings;

package Driverlib;

use File::Copy;
use File::Basename;
use FileHandle;
use POSIX ":sys_wait_h";
use POSIX qw(setpgid);
use POSIX ":signal_h";

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw(&report_result &system_with_timeout &printd &printq &progress &read_file $DEBUG $QUIET);

our $DEBUG = 0;
our $QUIET = 0;
our $PROGRESS = 0;
our ($LAST_CURRENT, $LAST_TOTAL) = (0, 0);
my ($wchar, $hchar, $wpixels, $hpixels);
eval {
  eval "use Term::ReadKey;";
  ($wchar, $hchar, $wpixels, $hpixels) = GetTerminalSize();
} or do {
  ($wchar, $hchar, $wpixels, $hpixels) = (1, 1, 1, 1);
};
our $TERM_WIDTH = $wchar;

#
# report_result - This is the routine that the driver calls when
#    it is time to output the autoresult string
#
sub report_result ($$) {
    my $result = shift;       # Autoresult string
    my $autograded = shift;   # Set if called by an autograder

    # Echo the autoresult string to stdout if the driver was called
    # by an autograder
    if ($autograded) {
        print "\n";
        print "AUTORESULT_STRING=$result\n";
        return;
    }
    else {
        print "Driver string :$result:\n";
    }
}

###
# system_with_timeout ($secs, $command, $outdest, $errdest)
# executes system($command) and returns its value
# or raises SIGALRM if it takes more than $secs
# taken from Perl 5.8.8 documentation of "alarm" and perlipc
#
# opens $outdest and $errdest as STDOUT and STDERR in the child
#
# possible alternative:
# $result = system("ulimit -t <timeout>; prog> /tmp/$$_temp_file 2>/dev/null");
#
sub system_with_timeout {
    my $secs = shift;
    my $command = shift;
    my $outdest = shift;
    my $errdest = shift;
    my $insrc = shift;
    my $result;
    my $kid;

    printd(2, "%% executing $command\n");
    my $p = fork();
    if ( $p == 0 ) {
        # child

        # redirect output
        if (defined $outdest) {
            close STDOUT;
            if (ref $outdest eq "GLOB") {
                open STDOUT, ">&", $outdest;
            }
            else {
                open STDOUT, $outdest;
            }
        }
        if (defined $errdest) {
            close STDERR;
            if (ref $errdest eq "GLOB") {
                open STDERR, ">&", $errdest;
            }
            else {
                open STDERR, $errdest;
            }
        }
        if (defined $insrc) {
            close STDIN;
            if (ref $insrc eq "GLOB") {
                open STDIN, "<&", $insrc;
            }
            else {
                open STDIN, $insrc;
            }
        }

        setpgid(0,0);
        exec("${command}");
        # I think the below comment is a little bit full of lies
        #
        # we need to do this because somehow sml manages to
        # fork a second internal process that ignores direct
        # kills of the child.  instead we create a new process
        # group and then send an INT process to the direct child
        # which then passes a KILL onto every process in its new group.
        die "exec failed";
    }

    # parent
    printd(2, "%% child process is $p, parent is $$\n");

    eval {
        local $SIG{ALRM} = sub { die "alarm" };
        local $SIG{INT} = sub { die "\nkilled" };
        alarm $secs;              # turn on alarm timer
        $kid = waitpid($p,0);     # blocking wait for child
        $result = $?;             # record result, if terminated normally
        alarm 0;                  # turn off timer
        printd(2,"%% process returned result $?\n");
    };

    ## once we've gotten here, it's time for the job to go.
    kill('KILL', -$p);      # kill all members of process group

    if (!$@) {                   # check whether "eval" died
        printd(2, "%% process $p returned normally\n");
        return $result;
    }

    if ($@ !~ /^alarm/) {
        die;                    # propagate exception if not "alarm"
    }

    ## We had a timeout
    printq(2, "%% Timeout after $secs seconds\n");
    $kid = 0;
    while ($kid >= 0) {     # 0 means some children are still running
        $kid = waitpid(-1, WNOHANG); # reap all members of process group
        if ($kid > 0) {
            printd(2, "%% reaped child $kid\n");
        }
    }
    printd(2, "%% reaped all children\n");
    return 128 + 14;              # return SIGALRM to signal timeout
}

###
# printd($level, $msg)
# print message if debug level >= $level
#
sub printd {
    my $level = shift;
    my $msg = shift;
    my $fh = shift;

    if (!defined $fh) {
        $fh = *STDOUT;
    }

    if ($DEBUG >= $level) {
        if ($PROGRESS) {
          print (' ' x $TERM_WIDTH. "\r");
        }
        print $fh $msg;
        if ($PROGRESS) {
            progress($LAST_CURRENT, $LAST_TOTAL);
        }
    }
}

###
# printq($level, $msg)
# print message if quiet level <= $level
#
sub printq {
    my $level = shift;
    my $msg = shift;
    my $fh = shift;

    if (!defined $fh) {
        $fh = *STDOUT;
    }

    if ($QUIET <= $level) {
        if ($PROGRESS) {
          print ("\r" . ' ' x $TERM_WIDTH . "\r");
        }
        print $fh $msg;
        if ($PROGRESS) {
            progress($LAST_CURRENT, $LAST_TOTAL);
        }
    }
}

###
# progress($current, $total, $width)
# prints a progress bar with the parameters given
#
sub progress {
    my $current = shift;
    my $total = shift;
    my $width = $TERM_WIDTH - 25;

    my $pct = $current/$total;
    printf "|%-${width}s| %5.1f%% (%d/%d)\r",
           '=' x ($pct * $width - 1) . '>',
           $pct * 100, $current, $total;
    $PROGRESS = 1;
    ($LAST_CURRENT, $LAST_TOTAL) = ($current, $total);
}

###
# read_file($file)
# returns file contents as a single string
#
sub read_file {
    my $file = shift;
    my $contents;
    open(INPUT, "<$file") or return undef;
    read(INPUT, $contents, -s INPUT);
    return $contents;
}

# End module with a 1 so that loading it returns TRUE
1;
