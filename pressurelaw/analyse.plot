set key autotitle columnhead
unset key
set datafile separator ","
ATMOSPHERE = 101.6
f(x) = m * (x - T_0)
FIT_LIMIT = 1e-9
fit f(x) 'data.csv' u 2:($1+ATMOSPHERE) via m, T_0
set title "Pressure law to find absolute 0"
set ylabel "Pressure / kPa"
set xlabel "Temperature / C"
set label 1 sprintf('T_0 = %.1f', T_0) at -300,100
set yrange [0:150]
set xrange [-400:100]
plot 'data.csv' u 2:($1+ATMOSPHERE), f(x)
