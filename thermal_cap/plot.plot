#!/usr/bin/env gnuplot

set key autotitle columnhead
#unset key
set datafile separator ","
set title "Thermal capacity"
set ylabel "temp / C"
set xlabel "t / s"
set yrange [20:45]
set xrange [0:2000]
plot 'data.csv' u 2:1
pause -1
