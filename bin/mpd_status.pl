#!/usr/bin/env perl
do{print substr($_,0,37)."...\n" if length($_)>39} or print "$_\n" for split/\n/,qx/ mpc -f "%artist%\n%title%\n%album%" current /;
