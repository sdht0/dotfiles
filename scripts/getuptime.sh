#!/bin/sh

boottime=`sysctl -n kern.boottime | awk '{print $4}' | sed 's/,//g'`
unixtime=`date +%s`
timeAgo=$(($unixtime - $boottime))
awk -v time=$timeAgo 'BEGIN { minutes = int(time / 60 % 60); hours = int(time / 60 / 60 % 24); days = int(time / 60 / 60 / 24); printf("%dd %dh %dm", days, hours, minutes); exit }'
