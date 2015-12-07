#!/bin/bash
#This is for running multiple game simulations in manageable chunks.
#Just specify the number of reps per simulation (50k takes 1h on a i5
#processor) and the random seed for reproducibility
#./runner 50000 1

reps=$1
i=$2

while true
do
    ./test --alphabet=polish --lexicon=osps --mode=playability --repetitions=$reps --seed=$i >> output/playability-reps$reps-seed$i
    ((i++))
done
