subroutine iniguess
use params
implicit none

integer ssc
! We modify the complete markets solution so to make it a good guess






do sc=1,ns
	do wc=1,pw
		do ssc=1,ns
		
		if (gridw(wc)<vmaut(sc) ) then

			gfun(sc,wc,ssc)=vmaut(ssc)
			hfun(sc,wc)=(vmaut(sc)-beta*sum(probs(sc,1:ns)*vmaut))/(1.0-beta)
			lafun(sc,wc)=0.0

		end if

		end do
	end do
end do


end subroutine iniguess