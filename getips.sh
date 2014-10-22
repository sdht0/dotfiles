#!/bin/sh

PATH="/sbin:/usr/sbin::$PATH"

ip -o -f inet addr | grep -v '127.0.0.1' | cut -d'/' -f1 | awk '{print "| "$4" ("$2")"}' | sort | uniq | tr '\n' ' '
printf "|"
