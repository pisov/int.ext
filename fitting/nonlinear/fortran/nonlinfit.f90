program nonlinfit
implicit none

double precision, parameter :: PI = 3.14159265359d0

double precision, dimension(:), allocatable :: x, y, sig
double precision, dimension(2) :: da, a, anew, dchi2, beta
double precision, dimension(2,2) :: hess
double precision :: lambda, chi2cur, chi2new

integer n, i, j, k, nprm
integer cnt, maxit, c1, c2, ok
integer :: pivot(2)

interface
function chi2(x, y, sig, n, a, nprm)
double precision :: chi2
double precision, dimension(:), intent(in) :: x, y, sig
integer, intent(in) :: n, nprm
double precision, dimension(:), intent(in) :: a
end function chi2
end interface

read(*,*)n

if ( n < 2) then
        write(0,*) 'Bad number of records = ',n
        stop
end if

a(1) = 195.e0
a(2) = 40.e0

nprm = 2

allocate(x(n))
allocate(y(n))
allocate(sig(n))

do i = 1, n
        read(*,*)x(i),y(i)
        sig(i) = 1.e0
end do

chi2cur = chi2(x, y, sig, n, a, nprm)

lambda = 1.d-5
cnt = 0
maxit = 5

do while ( cnt < maxit)
        cnt = cnt + 1
        !calc the hess matrix
        do j = 1, nprm
                
                do i = 1, nprm
                        hess(i,j) = 0.d0
                        
                end do
        end do

end do


deallocate(x)
deallocate(y)
deallocate(sig)


end program nonlinfit

function chi2(x, y, sig, n, a, nprm)
double precision :: chi2
double precision, dimension(:), intent(in) :: x, y, sig
integer, intent(in) :: n, nprm
double precision, dimension(:), intent(in) :: a

double precision, dimension(:), allocatable :: dy, res

double precision :: mu, sigg
!interface
!elemental function func(x, a, n) result(res)
!double precision :: res

!double precision, intent(in) :: x
!double precision, dimension(:), intent(in) ::  a
!integer, intent(in) :: n
!end function
!end interface

allocate(dy(n))
allocate(res(n))

mu = a(1)
sigg = a(2)

res = exp( -(x - mu)**2/(2*sigg**2) ) / (sigg * sqrt(2*PI))

dy(:) = y(:) - res

chi2 = sum(dy(:) * dy(:) / sig(:) * sig(:) )

deallocate(dy)
deallocate(res)

end function chi2

!elemental function func(x, a, n) result(res)
!double precision :: res

!double precision, intent(in) :: x
!double precision, dimension(:), intent(in) ::  a
!integer, intent(in) :: n



!double precision :: mu, sig
!double precision, parameter :: PI = 3.14159265359d0

!mu = a(1)
!sig = a(2)

!res = exp( -(x - mu)**2/(2*sig**2) ) / (sig * sqrt(2*PI))

!end function func

subroutine dyda(x, a, nsize, res)
double precision :: x
double precision, dimension(:), intent(in) :: a
integer, intent(in) :: nsize
double precision, dimension(nsize), intent(out) :: res

double precision :: mu, sig

res(:) = 0.d0
mu  = a(1)
sigg = a(2)

res = exp( -(x - mu)**2/(2*sigg**2) ) / (sigg * sqrt(2*PI))

res(1) = sum((x - mu) * res / sigg ** 2)
res(2) = - sum(( 1 + (mu - x) ** 2 / sig ** 2) * res / sigg)

end subroutine dyda
