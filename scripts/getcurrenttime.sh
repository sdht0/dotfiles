#!/bin/sh

time=$(date +'%Y.%m.%d') && \
echo -n $time | xclip -selection clipboard && \
notify-send "'$time' copied." || \
notify-send "Failed to get datetime."
