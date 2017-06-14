#!/bin/bash
for i in {26..100}
do
	eval "mkdir exp$i"
	for j in {0..100}
	do
		eval "time ./cubierta $j $i 20"
		eval "dot exp$i/grafica$j-$i-20.gv -Tsvg > exp$i/grafica$j-$i-20.svg"
	done
done
