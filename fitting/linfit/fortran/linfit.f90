program linfit
implicit none

integer :: n,i, alloc_stat
real, allocatable :: x(:), y(:)
real :: delta, s, sx, sy, sxx, sxy, ss
real :: a, b, siga,sigb, chi2, q

read(*,*)n

if (n > 2) then
  allocate(x(n), stat = alloc_stat)
  allocate(y(n), stat = alloc_stat)

  sx = 0.e0
  sy = 0.e0
  sxx = 0.e0

  do i = 1, n
  
    read(*,*)x(i),y(i)
    print *,x(i),y(i)
    sx = sx + x(i)
    sy = sy + y(i)
    sxx = sxx + x(i)*x(i)
    sxy = sxy + x(i)*y(i)
  end do

  !ss = 
  !delta = 
  !a = 
  !b = 
  !siga = 
  !sigb = 

  chi2 = 0.e0
  do i = 1, n
    !chi2 = 
  end do

  print *,'a= ', a, 'siga = ', siga, 'b= ', b, 'sigb= ', sigb
  print *,'chi2= ', chi2

!  print *,'ss= ', ss, 'delta= ', delta
!  print *,'sx= ', sx, 'sy= ', sy, 'sxx=', sxx, 'sxy= ', sxy

  deallocate(x)
  deallocate(y)  
else
  print *,"You should supply n > 2"
  stop
end if


end program

