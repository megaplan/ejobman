#!/usr/bin/perl
use strict;
my $n=1;
my $prog="/usr/bin/logger";
while(my $str = <STDIN>){
    `$prog -t "t.pl" "n=$n, str=$str"`;
    $n++;
    sleep 1;
}
`$prog -t "t.pl" "exit, n=$n"`;
