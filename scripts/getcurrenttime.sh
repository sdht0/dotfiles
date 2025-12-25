#!/bin/sh

time=$(date +'#date/%Y/%m/%d %a') && \
echo -n $time | xclip -selection clipboard && \
notify-send "'$time' copied." || \
notify-send "Failed to get datetime."
