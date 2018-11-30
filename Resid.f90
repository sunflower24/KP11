subroutine resid(x,fv)
use params

implicit none

real(prec) :: x,fv 

R=x

if (fsav==1) then    !	If there is saving in autarky then recompute the value of autarky
		call autvalsav
end if

! Solve for the planners decision rule, given the intertemporal price
call enforce

! compute stationary distribution for given intertemporal price
call diss

print*,'Stationary distribution computed'
! compute excess demand from given stationary distribution
call resource


fv=resd

end subroutine resid