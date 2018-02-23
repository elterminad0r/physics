if (!exists("graph_file")) graph_file='graph.png'
if (!exists("data_file")) data_file='results.dat'

set terminal png
set output graph_file

set title "Iterative model of falling object"
set xlabel "t/s"

plot data_file using 1:2 title "a/ms-2" with step, \
     data_file using 1:3 title "v/ms-1" with step, \
     data_file using 1:4 title "s/m" with step
