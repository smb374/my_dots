#!/usr/bin/env perl
open N,"/sys/class/backlight/intel_backlight/actual_brightness";
open M,"/sys/class/backlight/intel_backlight/max_brightness";
my$now=do{local $/;<N>};
my$max=do{local $/;<M>};
my$per=int(($now/$max+0.01)*100);
$per-=1 if $per==101;
print $per,"\n";
