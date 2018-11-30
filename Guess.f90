subroutine guess                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   

use params
implicit none
real(prec) kk


! fills the function with the complete markets solution	

do sc=1,ns
	do wc=1,pw

	kk=(beta*R)**((1.0-sigma)/sigma);

	gfun(sc,wc,1:ns)=gridw(wc)*kk
	hfun(sc,wc)=(gridw(wc)-beta*gfun(sc,wc,1))/(1.0-beta)
	lafun(sc,wc)=((R-1.0)/(R*(1.0-beta)))*MC(hfun(sc,wc))

	end do
end do





end subroutine guess