#!/bin/sh

run_segment() {
    printf "%s (%s users)" $(whoami) $(who | cut -d" " -f1 | sort | uniq | wc -l)
    return 0
}
