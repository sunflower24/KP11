subroutine sysex(x ,fv, n)
! this system deals with the exogenous incomplete markets

use params


implicit none
integer , intent(in) :: n
real(prec) , intent(in) ::x(n)
real(prec) , intent(out) :: fv(n)
real(prec) :: erhs,aprime,cons
real(prec) :: vals(2)
integer inds(2),i


 aprime=x(1)


 cons=Ri*grida(ac)+yat(sc)-aprime




	! compute  euler equations

	call basefun (grida,pa,aprime,vals,inds)
	erhs=0.0
	do i=1,ns
		erhs=erhs+probs(sc,i)*( vals(1)*vpfun(i,inds(1))+vals(2)*vpfun(i,inds(2)) )
	end do



		fv(1)=MU(cons)-beta*erhs










end subroutine sysex
