subroutine resourceexinc

use params
implicit none


real(prec),dimension(ns,pa)::adistr1
real(prec)::tote,cons,privint,govint,exass,resdc,exass1
real(prec),dimension(ns,pa)::enw, assdemand,assdemand1
privint=0.0
govint=0.0

! Unstack distribution

adistr1=0.0

do sc=1,ns
	adistr1(sc,1:pa)=adistr((sc-1)*pa+1:pa*sc)
end do

! construct total resources available in the economy

govint=0.0
privint=0.0
enw=0.0
aomega=0.0

do sc=1,ns
	do ac=1,pa
	
		enw(sc,ac)=yat(sc)*adistr1(sc,ac)
		privint=privint+abs(cifun(sc,ac)-yat(sc))*adistr1(sc,ac)
		govint=govint+abs(trates(sc)*states(sc)-g*states(sc))*adistr1(sc,ac)
		assdemand(sc,ac)=aifun(sc,ac)*adistr1(sc,ac)
		assdemand1(sc,ac)=grida(ac)*adistr1(sc,ac)


	end do
end do

tote=sum(enw)
exass=sum(assdemand)
exass1=sum(assdemand1)


cons=sum(adistr1*cifun)  ! total consumption
aomega=adistr1


resdc=cons-tote
resd=exass

print*,"consuption, endowment, redidual "
print*, cons, tote, resdc
print*,"Excess demand for bonds ",exass,exass1
print*,'Private intermediation ',privint
print*,'Govt intermediation ',govint
print*,'R is ',Ri
!pause
call condistexinc
!pause

privint2=1.0-stdlc/stdly
govint2=1.0-stdly/stdle
totint2=1.0-stdlc/stdle

print*,'Private intermediation (2)',privint2
print*,'Govt intermediation (2)',govint2
print*,'Total intermediation (2)',totint2

print*,govint2+(1.0-govint2)*privint2-totint2
!pause

end subroutine resourceexinc