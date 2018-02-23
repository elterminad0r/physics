.PHONY: show
show: graph.png
	gnome-open graph.png

.PHONY: fineshow
fineshow: finegraph.png
	gnome-open finegraph.png

graph.png: results.dat plot_results.plt
	gnuplot -p plot_results.plt

finegraph.png: fineresults.dat plot_results.plt
	gnuplot -p -e "graph_file='finegraph.png';data_file='fineresults.dat'" plot_results.plt 

fineresults.dat: get_results.py
	python get_results.py -m 3 -d 0.0001 > fineresults.dat

results.dat: get_results.py
	python get_results.py > results.dat
