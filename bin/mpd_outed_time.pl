#!/usr/bin/env perl
$_=qx/mpc/=~/(\w+):(\w{2})\/\w+:\w{2}\ /;
my$out=$1*60+$2;
print $out;
