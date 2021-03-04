#!/bin/sh

time=$(date +'%I:%M%P') && \
echo -n $time | xclip -selection clipboard && \
notify-send "Time '$time' copied to clipboard." || \
notify-send "Failed to get time."
