set xlabel "x"
set ylabel "f(x)"
set xrange [0:3.25]

plot   "polinom.dat" u 1:2 title "Calculated" w p pt 6 ps 0.5
replot "polinom.dat" u 1:4 title "Exact" w l

pause -1
