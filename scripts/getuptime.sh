#!/bin/sh

awk '{printf("%dd %02dh %02dm",($1/60/60/24),($1/60/60%24),($1/60%60))}' /proc/uptime
echo

