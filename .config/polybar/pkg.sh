#!/bin/bash
pac=$(checkupdates | wc -l)
aur=$(cower -u | wc -l)

check=$((pac + aur))
echo "$pac %{F#eef1f6}%{F-} $aur"
