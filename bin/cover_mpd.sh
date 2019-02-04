#!/usr/bin/zsh

filename="cover_mpd.png";
placeholder="/home/thomas/placeholder.png"
fil="/home/thomas/Music/$(mpc --format %file% current)"
album=$(dirname "$fil");
na=$(mpc --format %album% current);
ca=$(cat /tmp/album);
cover=$album"/"`ls $album | grep -Pi "cover[^0-9]|folder[^0-9]" | grep -Ei "jpeg|jpg|png|gif" | head -n 1`;
if [[ ! -f /tmp/album ]]; then
    echo $(mpc --format %album% current) > /tmp/album;
    if [[ ! -f $cover ]]; then
        cp $placeholder /tmp/$filename;
    else
        convert $cover -resize "$1>x" /tmp/$filename;
    fi
elif [[ $na != $ca ]]; then
    echo $na > /tmp/album;
    if [[ ! -f $cover ]]; then
        cp $placeholder /tmp/$filename;
    else
        convert $cover -resize "$1>x" /tmp/$filename;
    fi
else
    echo $na > /tmp/album;
    if [[ ! -f $cover ]]; then
        cp $placeholder /tmp/$filename;
    else
        convert $cover -resize "$1>x" /tmp/$filename;
    fi
fi
