subroutine sys1(x ,fv, n)
! this system deals with the case in which PK is binding and DC are all slack

use params


implicit none
integer , intent(in) :: n
real(prec) , intent(in) ::x(n)
real(prec) , intent(out) :: fv(n)
real(prec),dimension(ns) :: erhs,elhs,gh
real(prec) :: npla,vals(2),hh
integer inds(2),i


 gh(1:ns)=x(1:ns)
 hh=(gridw(wc)- sum( probs(sc,1:ns)*beta*gh(1:ns) ) )/(1.0-beta)



do i=1,ns


	! compute  euler equations

	call basefun (gridw,pw,gh(i),vals,inds)

	npla=vals(1)*lafun(i,inds(1))+vals(2)*lafun(i,inds(2))
	erhs(i)=npla
	elhs(i)=beta*(R-1.0)*MC(hh)/(1.0-beta)





		fv(i)=elhs(i)-erhs(i)


end do







end subroutine sys1
