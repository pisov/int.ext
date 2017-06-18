set size ratio 1
set xrange [0:3.25]
set xlabel  "x"
set ylabel "y(x)"

plot   "plot.dat"   u 1:2 w l      title "Cubic spline"
replot "points.dat" u 1:2 w p pt 7 title "Data points "

pause -1
