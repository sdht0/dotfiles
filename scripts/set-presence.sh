#!/bin/sh

[[ "$1" != "auto" ]] && [[ "$1" != "away" ]] && { echo "Invalid argument"; exit 1; }

curl -X POST -H 'Authorization: Bearer xoxp-xxx' --data "presence=$1" "https://slack.com/api/users.setPresence" && \
curl -X POST -H 'Authorization: Bearer xoxp-xxy' --data "presence=$1" "https://slack.com/api/users.setPresence" && \
notify-send "Presence is now '$1'." || \
notify-send "Setting presence to '$1' failed."
