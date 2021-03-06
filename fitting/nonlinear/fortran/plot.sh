#!/bin/bash

cat > plot.gnu <<- "EOF"
f(x,mu, sig) = exp(-(x-mu)*(x-mu)*0.5/(sig*sig))/(sqrt(2*3.14159265)*sig)
set xrange [0:400]
set xlabel "x"
set ylabel "f(x)"

set size ratio 1

plot "gauss.dat" u 1:2 w boxes title "Data"
EOF

printf 'replot f(x, %f, %f) w l lw 2 title "Fit"' $1 $2 >> plot.gnu
printf '\npause -1' >> plot.gnu

gnuplot plot.gnu
