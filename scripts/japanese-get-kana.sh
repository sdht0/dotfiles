#!/bin/bash

env | sort

strn=$(xclip -out clipboard) && \
[[ -n "$strn" ]] && \
output="$(/home/sdh/Downloads/installations/anaconda3/envs/sudachi/bin/python /home/sdh/.dotfiles/scripts/japanese-get-kana.py $strn)" && \
[[ -n "$output" ]] && \
echo -n "$output" && \
echo -n "$output" | xclip -selection clipboard && \
notify-send "Kana '$output' copied." || \
notify-send "Failed to get kana."
