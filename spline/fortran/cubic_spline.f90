program cubicspline
implicit none

interface
  subroutine splint(x, y, ysec)
    double precision, dimension(:), intent(in) :: x, y
    double precision, dimension(:), intent(out) :: ysec
  end subroutine splint
  function spline(x, y, ysec, xx)
  double precision, dimension(:), intent(in) :: x, y, ysec
  double precision :: xx, spline
  end function spline
end interface


integer :: i, j, n, np
double precision, allocatable, dimension(:) :: x, y, ysec
double precision :: xx, yy, h, xmin, xmax

write(0,*)'np = '
read(*,*)np

n = 5

allocate(x(n))
allocate(y(n))
allocate(ysec(n))

x = (/0.25d0, 0.67d0, 1.d0, 1.5d0, 3.d0/)
y = (/0.779d0, 0.994d0, 0.939d0, 0.57d0, 0.006d0/)

call splint(x, y, ysec)

xmin = minval(x)
xmax = maxval(x)
h = (xmax - xmin) / (np - 1)

do i = 0, np - 1
  xx = i * h + xmin
  write(*,'(2F10.5)')xx,spline(x,y,ysec,xx)
end do

deallocate(x)
deallocate(y)
deallocate(ysec)

end program cubicspline

subroutine splint(x, y, ysec)
implicit none
double precision, dimension(:), intent(in) :: x, y
double precision, dimension(:), intent(out) :: ysec

double precision, dimension(size(x)) :: d, dl, du
integer, dimension(size(x)) :: ipiv
integer :: i, n, info

n = size(x)

d(1:n-2)  = (x(3:n)   - x(1:n-2))/3.d0
dl(1:n-2) = (x(2:n-1) - x(1:n-2))/6.d0
du(1:n-2) = (x(3:n)   - x(2:n-1))/6.d0

ysec(2:n-1) = (y(3:n) - y(2:n-1))/(x(3:n)-x(2:n-1)) - (y(2:n-1)-y(1:n-2))/(x(2:n-1)-x(1:n-2))


call dgtsv (n-2, 1, dl, d, du, ysec(2:n-1), n-2, info)

ysec(1) = 0.d0
ysec(n) = 0.d0

end subroutine splint

function spline(x, y, ysec, xx)
implicit none
double precision, dimension(:), intent(in) :: x, y, ysec
double precision :: xx, spline

integer :: i, idx, n
double precision :: a, b, c, d, dx, dxmin

n = size(x)

idx = 1
dxmin = xx - x(1)
do i = 2, n - 1
  dx = xx - x(i)
  if (dx.gt.0.d0 .and.  dx .lt.dxmin) then
    idx = i
    dxmin = dx
  end if
end do


a = (x(idx+1) - xx)/(x(idx+1)-x(idx))
b = 1.d0 - a
c = (a**3-a)*(x(idx+1)-x(idx))**2
d = (b**3-b)*(x(idx+1)-x(idx))**2
spline = a*y(idx)+b*y(idx+1)+(c*ysec(idx)+d*ysec(idx+1))/6.d0
end function spline
