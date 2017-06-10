#!/bin/bash
for i in {4..10}
do
    eval "time ./cuvierta $i 100"
done
