subroutine syssav(x ,fv, n)
! this system deals with the exogenous incomplete markets

use params


implicit none
integer , intent(in) :: n
real(prec) , intent(in) ::x(n)
real(prec) , intent(out) :: fv(n)
real(prec) :: erhs,sprime,cons
real(prec) :: vals(2)
integer inds(2),i


 sprime=x(1)


 cons=Rm*gridm(rc)+yat(sc)-sprime




	! compute  euler equations

	call basefun (gridm,pm,sprime,vals,inds)
	erhs=0.0
	do i=1,ns
		erhs=erhs+probs(sc,i)*( vals(1)*vpmfun(i,inds(1))+vals(2)*vpmfun(i,inds(2)) )
	end do



		fv(1)=MU(cons)-beta*erhs










end subroutine syssav
