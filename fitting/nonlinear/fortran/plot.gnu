f(x,mu, sig) = exp(-(x-mu)*(x-mu)*0.5/(sig*sig))/(sqrt(2*3.14159265)*sig)
set xrange [0:400]
set xlabel "x"
set ylabel "f(x)"

set size ratio 1

plot "gauss.dat" u 1:2 w boxes title "Data"
replot f(x, 195.000000, 45.000000) w l lw 2 title "Fit"
pause -1