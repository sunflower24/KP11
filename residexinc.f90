subroutine residexinc(x,fv)
use params

implicit none

real(prec) :: x,fv 

Ri=x

! Solve for the agents decision rules, given the intertemporal price
call enforceexinc

! compute stationary distribution for given intertemporal price

call dissexinc

print*,'Stationary distribution computed'

! compute excess demand from given stationary distribution
call resourceexinc
! pause

fv=resd

end subroutine residexinc