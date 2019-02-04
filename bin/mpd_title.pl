#!/usr/bin/env perl
my$mpd_title=qx/mpc -f %title% current/;
my$title=$mpd_title;
$title=substr($mpd_title,0,36)."..." if length($mpd_title)>40;
print $title;
