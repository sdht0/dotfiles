#!/bin/sh

. ~/.bashrc && \
strn=$(xclip -out clipboard) && \
email=$(xegen $strn) && \
echo $email | xclip -selection clipboard && \
notify-send "Email '$email' copied to clipboard." || \
notify-send "Generating email for '$strn' failed."
