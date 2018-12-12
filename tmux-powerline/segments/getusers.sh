#!/bin/sh

run_segment() {
    printf "v%s %s (%s users)" $(uname -r | cut -d- -f1) $(whoami) $(who | cut -d" " -f1 | sort | uniq | wc -l)
    return 0
}
