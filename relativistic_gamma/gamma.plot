#!/usr/bin/env gnuplot

set terminal postscript portrait enhanced color dashed lw 1 "DejaVuSans" 12

set output "gamma.ps"

unset key

set title "Gamma factor at relativistic speeds"
set ylabel '\gamma'
set xlabel 'Speed / c'
set yrange [1:10]
set xrange [0:1]

plot 1 / sqrt(1 - x ** 2)

