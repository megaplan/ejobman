#!/usr/bin/perl
use strict;
use IO::Handle;
STDERR->autoflush(1);
STDOUT->autoflush(1);
#sleep 1;
print "cur_pid=$$\n";
my $n=1;
my $prog="/usr/bin/logger";
while(my $str = <STDIN>){
    `$prog -t "t.pl" "n=$n, str=$str"`;
    $n++;
    sleep 1;
}
`$prog -t "t.pl" "exit, pid=$$, n=$n"`;
