#!/bin/sh

run_segment() {
    PATH="/sbin:/usr/sbin::$PATH"
    ip -o -f inet addr  | cut -d'/' -f1 | sed -r "s/[ \t]+/ /g" | cut -d" " -f2-4 | awk '{print " "$3" ("$1") "}' | sort | uniq | head -n 2 | paste -sd "|" -
    return 0
}
