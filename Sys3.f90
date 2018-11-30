subroutine sys3(x ,fv, n)
! this system deals with the case low is slack, middle and high are binding

use params


implicit none
integer , intent(in) :: n
real(prec) , intent(in) ::x(n)
real(prec) , intent(out) :: fv(n)
real(prec) :: hh,gh(ns),npla,vals(2),erhs(1),elhs(1)
integer inds(2),i


 gh(1)=x(1)
 gh(2:ns)=vmaut(2:ns)
 

 hh= (gridw(wc) - beta*(sum(probs(sc,1:ns)*gh)))/(1.0-beta) 



do i=1,ns-2

	! compute  euler equations

	call basefun (gridw,pw,gh(i),vals,inds)

	npla=vals(1)*lafun(i,inds(1))+vals(2)*lafun(i,inds(2))
	erhs(i)=npla
	elhs(i)=beta*(R-1.0)*MC(hh)/(1.0-beta)





	fv(i)=elhs(i)-erhs(i)


end do











	


end subroutine sys3
