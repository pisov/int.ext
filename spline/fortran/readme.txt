1. Compile

gfortran -O3 cubic_spline.f90 -o cubic.x -llapack

or

make

2. Execute

./cubic.x > plot.dat

3. Plot the interpolated curve

gnuplot plot.gnu

[Exersize:]

Modify existing code in order to accept as parameters the first derivative at the interval boundaries
