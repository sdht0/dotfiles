#!/bin/sh

file=id_ed25519.pub
cat ~/.ssh/$file | wl-copy && \
notify-send "SSH key \"$file\" copied to clipboard." || \
notify-send "SSH key \"$file\" copying failed."
