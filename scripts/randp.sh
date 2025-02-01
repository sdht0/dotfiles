#!/bin/sh

password=$(python ~/.config/dotfiles/scripts/password.py | tr -d '\n')
echo -n "$password" | xclip -selection clipboard && \
notify-send "Password of length ${#password} copied to clipboard." || \
notify-send "Failed to generate new password."
