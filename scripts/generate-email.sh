#!/bin/sh

. ~/.bashrc && \
strn=$(xclip -out clipboard | tr '[:upper:]' '[:lower:]') && \
email=$(xegen $strn) && \
echo -n $email | xclip -selection clipboard && \
notify-send "Email '$email' copied to clipboard." || \
notify-send "Generating email for '$strn' failed."
