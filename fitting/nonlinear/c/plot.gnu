set xrange [0:400]
set xlabel "x"
set ylabel "f(x)"

set size ratio 1

plot "gauss.dat" u 1:2 w boxes title "Data"

pause -1
