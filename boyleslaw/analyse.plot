set terminal epslatex color
unset key
ROOM_TEMP = 25 + 273.15
MOLAR_CONST = 8.3144598
f(x) = n * MOLAR_CONST * ROOM_TEMP * x + c
FIT_LIMIT = 1e-9
fit f(x) 'data.tsv' u (0.001/($1 + 101.6 + 0.3)):($2/1000000) via n, c
set title "Number of moles in a gas sample from Boyle's law"
set ylabel '$V$/\si{m^3}' offset -1.6,0
set xlabel '$\frac{1}{p}$/\si{Pa^{-1}}'
set label 1 sprintf('$n$ = \SI{%.5f \pm %.5f}{\mol}', n, n_err) at 2e-6,5e-5
set label 2 sprintf('$V_{extra}$ = %.2e \si{\milli \litre}', c) at 2e-6,4e-5
set yrange [0:7e-5]
set xrange [0:1.2e-5]
plot 'data.tsv' u (0.001/($1 + 101.6 + 0.3)):($2/1000000), f(x)
