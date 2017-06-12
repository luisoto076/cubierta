#!/bin/bash
for i in {0..100}
do
    eval "time ./cubierta $i 0 10"
    eval "dot grafica$i-0-10.gv -Tsvg > grafica$i-0-10.svg"
done
