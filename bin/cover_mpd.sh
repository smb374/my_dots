#!/usr/bin/zsh

filename="cover_mpd.png"
placeholder="/home/thomas/placeholder.png"
file="/home/thomas/Music/$(mpc --format %file% current)" # current music file path
album=$(dirname "$file") # current album dir
na=$(mpc --format %album% current) # current album name
ca=$(cat /tmp/album) # cached album name
cover=$album"/"`ls $album | grep -Pi "cover[^0-9]|folder[^0-9]" | grep -Ei "jpeg|jpg|png|gif" | head -n 1`;

if [[ ! -f /tmp/album ]]; then
    echo $na > /tmp/album;
    if [[ ! -f $cover ]]; then
        cp $placeholder /tmp/$filename;
    else
        convert $cover -alpha set -background none -channel A -evaluate multiply 0.75 +channel -resize "$1>x" /tmp/$filename;
    fi
fi

if [[ ! -f /tmp/$filename ]];then
    echo $na > /tmp/album;
    if [[ ! -f $cover ]]; then
        cp $placeholder /tmp/$filename;
    else
        convert $cover -alpha set -background none -channel A -evaluate multiply 0.75 +channel -resize "$1>x" /tmp/$filename;
    fi
fi

if [[ $na != $ca ]]; then
    echo $na > /tmp/album;
    if [[ ! -f $cover ]]; then
        cp $placeholder /tmp/$filename;
    else
        convert $cover -alpha set -background none -channel A -evaluate multiply 0.75 +channel -resize "$1>x" /tmp/$filename;
    fi
fi
