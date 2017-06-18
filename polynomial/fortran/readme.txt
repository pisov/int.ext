In order to compile the example please do following steps

1. Compile the example code

gfortran -O3 poly.f90 utils.f90 -o poly.x

or

make

2. Execute example

./poly.x > polinom.dat

3. Plot calculated function and expected on

gnuplot plot.gnu

4. Modify the source code poly.f90 in order to apply the five point interpolation scheme for following points

0.25, 0.67, 1.0, 1.5, 3.0

5. Replot the interpolation curve


