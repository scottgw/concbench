#!/bin/bash
source run_func.sh

# 'barrier' missing from below
benchmarks=(condition mutex noshare prodcons share)

for benchmark in "${benchmarks[@]}"
do
    run $benchmark
done
