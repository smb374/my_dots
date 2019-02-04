#!/usr/bin/zsh

filename="cover.png";
placeholder="/home/thomas/placeholder.png"
album=`audtool current-song-tuple-data file-path | sed 's/\~/\/home\/thomas/'`;
na=`audtool current-song-tuple-data album`;
ca=`cat /tmp/album`;
cover=$album"/"`ls $album | grep -Pi "cover[^0-9]|folder[^0-9]" | grep -Ei "jpeg|jpg|png|gif" | head -n 1`;
if [[ ! -f /tmp/album ]]; then
    echo $(audtool current-song-tuple-data album) > /tmp/album;
    if [[ ! -f $cover ]]; then
        cp $placeholder /tmp/$filename;
    else
        convert $cover -resize "130>x" /tmp/$filename;
    fi
elif [ $na != $ca ]; then
    echo $na > /tmp/album;
    if [[ ! -f $cover ]]; then
        cp $placeholder /tmp/$filename;
    else
        convert $cover -resize "130>x" /tmp/$filename;
    fi
else
    echo $na > /tmp/album;
    if [[ ! -f $cover ]]; then
        cp $placeholder /tmp/$filename;
    else
        convert $cover -resize "130>x" /tmp/$filename;
    fi
fi
