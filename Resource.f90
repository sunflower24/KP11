subroutine resource

use params
implicit none


integer:: scc
real(prec),dimension(ns,pw)::distr1
real(prec)::tote,cons,privint,govint
real(prec),dimension(ns,pw)::enw
privint=0.0
govint=0.0

print*, "resource started"
! construct consumption function

do sc=1,ns
	do wc=1,pw
	
			 cfun(sc,wc)=C(hfun(sc,wc))
	end do
end do

! unstack distribution over (y,w)

do sc=1,ns
distr1(sc,1:pw)=distr((sc-1)*pw+1:pw*sc)
end do

! construct total resources available in the economy

do sc=1,ns
	do wc=1,pw
	
		enw(sc,wc)=yat(sc)*distr1(sc,wc)
		privint=privint+abs(cfun(sc,wc)-yat(sc))*distr1(sc,wc)
		govint=govint+abs(trates(sc)*states(sc)-g*states(sc))*distr1(sc,wc)
		



	end do
end do

tote=sum(enw)

cons=sum(distr1*cfun)  ! total consumption
omega=distr1


resd=cons-tote
call condist



privint2=1.0-stdlc/stdly
govint2=1.0-stdly/stdle
totint2=1.0-stdlc/stdle

print*,"consuption, endowment, redidual "
print*, cons, tote, resd
print*,'Private intermediation (1)',privint
print*,'Govt intermediation (1)',govint

print*,'Private intermediation (2)',privint2
print*,'Govt intermediation (2)',govint2
print*,'Total intermediation (2)',totint2






end subroutine resource   