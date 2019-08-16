#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs
#
# if [ -d /etc/X11/xinit/xinitrc.d ] ; then
#  for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
#   [ -x "$f" ] && . "$f"
#  done
#  unset f
# fi
dwm_startup () {
    dunst &
    feh --bg-fill '/home/thomas/Pictures/Wallpapers/34871417_p0.jpg' &
    compton &
    nm-applet &
    aria2c --conf-path=/home/thomas/.config/aria2/aria2.conf &
    fcitx &
    clipit &
    xss-lock -- betterlockscreen -l dim &
    conky -c ~/.i3.conkyrc -dq &
    lxpolkit &
    st -c neomutt -n neomutt -e neomutt &
    st -c weechat -n weechat -e weechat &
    st -g 150x10 -c ncmpcpp -n ncmpcpp -e ncmpcpp &
    slstatus &
    ~/dwm/dwm 2> ~/.dwm.log
}
spectrwm_startup () {
    dunst &
    feh --bg-fill '/home/thomas/Pictures/Wallpapers/berserker_in_the_moonlight_by_fazal_sama_d95sn3c.jpg'
    conky -c ~/.i3.conkyrc -dq
    bash -c "$HOME/.config/polybar/launch_s.sh"
    compton &
    aria2c --conf-path=/home/thomas/.config/aria2/aria2.conf &
    fcitx &
    clipit &
    xss-lock -- betterlockscreen -l dim &
    lxpolkit &
    exec spectrwm
}
#xrandr --setprovideroutputsource modesetting NVIDIA-0
xrdb ~/.Xresources
xrandr --auto
# wal -R
# exec i3
# exec dwm
xsetroot -cursor_name left_ptr
export MONITOR=$(polybar -m|tail -1|sed -e 's/:.*$//g')
xset m 0 0
# exec $HOME/.xmonad/xmonad-x86_64-linux
# systemctl --user restart mpd 2>/dev/null
exec ~/.local/bin/xmonad
# exec i3
# dwm_startup
# exec stumpwm.ros
# spectrwm_startup
# exec awesome
