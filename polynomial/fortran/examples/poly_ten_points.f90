PROGRAM neville
USE utils
IMPLICIT NONE

REAL, DIMENSION(10) :: x, y
REAL :: h,xmin, xmax, xx, yy, erry, step
INTEGER i, n, alloc_stat

xmin = 5.e-1
xmax = 1.e0

x = (/0.25, 0.5 , 0.67, 0.7 , 0.74 , 1.0, 1.5, 2.3, 2.88, 3.0/)
y(:) =  exp(-(x(:)- 0.75e0)**2)

xmin = minval(x(:))
xmax = maxval(x(:))

n = 20
step = (xmax - xmin) / n


DO i = 0, n
  xx = xmin + step * i
  call polint(x,y,xx,yy,erry)
  print *,xx,yy,erry,exp(-(xx-0.75e0)**2)
END DO


END PROGRAM neville
