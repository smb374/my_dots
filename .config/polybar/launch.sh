#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# Launch bar
MONITOR=$(polybar -m|tail -n 1|sed -e 's/:.*$//g') polybar -r top &
MONITOR=$(polybar -m|tail -n 1|sed -e 's/:.*$//g') polybar -r bottom &

echo "Bars launched..."
