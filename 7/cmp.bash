#!/bin/bash

if [ $# -ne 2 ]; 
    then
    echo "usage: $ ./cmp.bash ./<DADLimplementation.executable> ./<DADLmodel.executable> ";
    exit
fi

EXE=$1
MOD=$2

if [ ! -d tmp/ ];
then 
    mkdir tmp
fi

for f in Tests/*in*.xml 
do
    echo $f
    rm -rf tmp/*
    $EXE < $f > tmp/exe.tmp
    $MOD < $f > tmp/mod.tmp 
    diff tmp/exe.tmp tmp/mod.tmp
done
