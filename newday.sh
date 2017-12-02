#!/bin/bash

if [[ -z "$1" ]] ; then
    echo "Please provide a numeric argument!" >&2
    exit 1
fi

install -D template/dayn.cabal day${1}/day${1}.cabal
install -D template/src/Main.hs day${1}/src/Main.hs
install -D template/src/Dayn.hs day${1}/src/Day${1}.hs
install -D template/test/Testn.hs day${1}/test/Test${1}.hs

sed -i "s/Dayn/Day${1}/; s/dayn/day${1}/; s/Testn/Test${1}/" $(find day${1} -type f)
