subroutine loadfun
use params
implicit none
integer ssc,inds(2)
real(prec) vals(2)




open(11)
	rewind(11)
	read (11,*) gfun, hfun, lafun,vfun,gridw
	rewind(11)

	if ( maxval(abs(gridw-ngridw)) > 0.001 ) then
		print*,'Grid is changed'
		pause

		do sc=1,ns
			do wc=1,pw

		
			call basefun (gridw,pw,ngridw(wc),vals,inds)

			lafun(sc,wc)=vals(1)*lafun(sc,inds(1))+vals(2)*lafun(sc,inds(2))
			hfun(sc,wc)=vals(1)*hfun(sc,inds(1))+vals(2)*hfun(sc,inds(2))
		
				do ssc=1,ns
					gfun(sc,wc,ssc)=vals(1)*gfun(sc,inds(1),ssc)+vals(2)*gfun(sc,inds(2),ssc)
				end do

			end do
		end do

		call dgrid

	end if




close(11)
end subroutine loadfun