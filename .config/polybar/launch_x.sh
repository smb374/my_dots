#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# Launch bar
MONITOR=$(polybar -m|tail -1|sed -e 's/:.*$//g') polybar -r -c ~/.config/polybar/config_xmonad top &
MONITOR=$(polybar -m|tail -1|sed -e 's/:.*$//g') polybar -r -c ~/.config/polybar/config_xmonad bottom &

echo "Bars launched..."
