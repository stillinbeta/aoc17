#!/bin/bash

stack install
export PATH=$HOME/.local/bin:$PATH

for dayDir in $(find . -maxdepth 1 -name 'day*' -type d  | sort -t y -k 2 -g); do
    day=$(basename $dayDir)
    echo "===${day}==="
    ${day} < ${dayDir}/input.txt
done
