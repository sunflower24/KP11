subroutine exincmarkets
! This subroutine compute the exogenously incomplete market economy

use params
implicit none
real ccm

external residexinc

Ri=1.02

! Initial guesses


	

	do ac=1,pa
		do sc=1,ns

			ccm=sum(yat*stae)
			if (yat(sc)+Ri*grida(ac)-ccm >= blimit ) then  
			cifun(sc,ac)=ccm
			else
			cifun(sc,ac)=yat(sc)+Ri*grida(ac)-blimit
			end if

			vpfun(sc,ac)=Ri*MU(cifun(sc,ac))
			aifun(sc,ac)=yat(sc)+Ri*grida(ac)-cifun(sc,ac)
			
		end do
	end do




!Compute Equilibrium Interest Rate for Exogenous Incomplete Markets

call Newton(residexinc,Ri)

print*,'Equilibrium interest rate '
print*,Ri
pause
call condistexinc
call cgini(gridc,cidist,ginico)
pause

end subroutine exincmarkets