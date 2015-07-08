#!/bin/sh

PATH="/sbin:/usr/sbin::$PATH"

ip -o -f inet addr | grep -v '127.0.0.1' | cut -d'/' -f1 | sed -r "s/[ \t]+/ /g" | cut -d" " -f2-4 | sort | uniq | awk '{print "| "$3" ("$1")"}' | sort | head -n 3 | tr '\n' ' '
printf "|"
