#!/bin/sh

PATH="/sbin:/usr/sbin::$PATH"

ip -o -f inet addr | grep -v '127.0.0.1' | cut -d'/' -f1 | cut -d" " -f2- | sort | awk '{print "| "$3" ("$1")"}' | sort | uniq | tr '\n' ' '
printf "|"
