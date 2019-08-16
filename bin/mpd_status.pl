#!/usr/bin/env perl
do{print substr($_,0,44)."...\n" if length($_)>46} or print "$_\n" for split/\n/,qx/ mpc --host=127.0.0.1 -p 6600 -f "%artist%\n%title%\n%album%" current /;
