program nonlinfit
  implicit none

  double precision, parameter :: PI = 3.14159265359d0

  double precision, dimension(:), allocatable :: x, y, sig
  double precision, dimension(2) :: da, a, anew, dchi2, beta
  double precision, dimension(2,2) :: hess
  double precision :: lambda, chi2cur, chi2new, yk, eps

  integer n, i, j, k, nprm, info
  integer cnt, maxit, c1, c2, ok
  integer :: pivot(2)

  interface
    function chi2(x, y, sig, n, a, nprm)
      double precision :: chi2
      double precision, dimension(:), intent(in) :: x, y, sig
      integer, intent(in) :: n, nprm
      double precision, dimension(:), intent(in) :: a
    end function chi2
    subroutine dyda(x, a, nsize, res)
      implicit none
      double precision :: x
      double precision, dimension(:), intent(in) :: a
      integer, intent(in) :: nsize
      double precision, dimension(nsize), intent(out) :: res
    end subroutine dyda
  end interface

  ! Read the number of data points from standard input
  read(*,*)n

  if ( n < 2) then
    write(0,*) 'Bad number of records = ',n
    stop
  end if

  ! Initialiaztion of fit parameters
  ! a(1) -> mu
  a(1) = 195.e0
  ! a(2) -> sig
  a(2) = 45.e0

  ! Number of fit parameters
  nprm = 2

  ! Allocate data arrays
  allocate(x(n))
  allocate(y(n))
  allocate(sig(n))

  ! Read the expected data points from standard input
  do i = 1, n
    read(*,*)x(i),y(i)
    sig(i) = 1.e0
  end do

  chi2cur = chi2(x, y, sig, n, a, nprm)

  lambda = 1.d-5
  eps = 1.d-5
  cnt = 0
  maxit = 15 
  beta(:) = 0.d0

  ! Main iteration loop
  write(0,'(I3,4E20.7)')cnt,(a(j),j=1, nprm)
  do 
    !calc the hess matrix
    do j = 1, nprm
      do i = 1, nprm
          !hess(i,j)  = 
      end do
    end do

    call dgesv(nprm,1,hess, nprm,pivot, beta, nprm, info) 
    if ((cnt > maxit).or.(sqrt(dot_product(beta, beta)).lt.eps)) exit
    cnt = cnt + 1
    a(:) = a(:) + beta(:) 
    write(0,'(I3,4E20.7)')cnt,(a(j),j=1, nprm)
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

subroutine dyda(x, a, nsize, res)
  implicit none
  double precision :: x
  double precision, dimension(:), intent(in) :: a
  integer, intent(in) :: nsize
  double precision, dimension(nsize), intent(out) :: res

  double precision :: mu, sigg, fnc
  double precision, parameter :: PI = 3.14159265359d0

  res(:) = 0.d0
  mu  = a(1)
  sigg = a(2)

  fnc = exp( -(x - mu)**2/(2*sigg**2) ) / (sigg * sqrt(2*PI))

  res(1) = (x - mu) * fnc / sigg ** 2
  res(2) = fnc * ((mu - x) ** 2 - sigg ** 2) / sigg ** 3
end subroutine dyda
