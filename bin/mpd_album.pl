#!/usr/bin/env perl
my$mpd_artist=qx/mpc -f %album% current/;
my$artist=$mpd_artist;
$artist=substr($mpd_artist,0,38)."..." if length($mpd_artist)>40;
print $artist;
