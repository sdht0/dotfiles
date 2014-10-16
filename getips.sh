#!/bin/sh

printf "|";

for i in $(ip addr | grep "^[0-9]*:" | cut -d: -f2 | grep -v lo | sed "s/ //");do
    x=$(ip addr show $i | grep "inet " | grep -v "127.0.0.1" | cut -d/ -f1 | awk "{ print \$2}");[[ -n "$x" ]] && printf " $x ($i) |" ;
done