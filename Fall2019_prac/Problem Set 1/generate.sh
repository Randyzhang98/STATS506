#!/bin/bash
mkdir generate
chmod -x generate
for i in `seq 0 5`
do
    for j in `seq 1 6`
    do
        ./generate test$i.$j $i $j 5
    done
done 


