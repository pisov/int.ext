MODULE utils
CONTAINS
SUBROUTINE polint(xa,ya,x,y,dy)
IMPLICIT NONE
REAL, DIMENSION(:), INTENT(IN) :: xa,ya
REAL, INTENT(IN) :: x
REAL, INTENT(OUT) :: y,dy
INTEGER :: m,n,ns
INTEGER, DIMENSION(1) :: indx
REAL, DIMENSION(size(xa)) :: c,d,den,ho
n=size(xa)
c=ya
d=ya
ho=xa-x
indx=minloc(abs(x-xa))
ns=indx(1)
y=ya(ns)
ns=ns-1
do m=1,n-1
	den(1:n-m)=ho(1:n-m)-ho(1+m:n)
	if (any(den(1:n-m) == 0.0)) then
		write(*,*)'polint: calculation failure'
                stop
        end if
	den(1:n-m)=(c(2:n-m+1)-d(1:n-m))/den(1:n-m)
	d(1:n-m)=ho(1+m:n)*den(1:n-m)
	c(1:n-m)=ho(1:n-m)*den(1:n-m)
	if (2*ns < n-m) then
		dy=c(ns+1)
	else
		dy=d(ns)
		ns=ns-1
	end if
	y=y+dy
end do
END SUBROUTINE polint
END MODULE utils
