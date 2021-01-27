#!/bin/bash

strn=$(xclip -out clipboard) && \
[[ -n "$strn" ]] && \
output=$(python ~/.dotfiles/scripts/japanese-get-kana.py "$strn") && \
[[ -n "$output" ]] && \
echo -n "$output" | xclip -selection clipboard && \
notify-send "Kana '$output' copied." || \
notify-send "Failed to get kana."
