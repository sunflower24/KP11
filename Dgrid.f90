subroutine dgrid  ! Set the grid for the enforcement and exogenous incomplete mkts
use params

implicit none
real(prec) :: wlb,whb,ustepsize,lstepsize,center,wwc
integer,parameter :: plhgw=pw/2		 ! number of points  in the lower part
integer :: phhgw=pw-1-plhgw			 ! number of points  in the higher part



real(prec) ,dimension(plhgw)::lhgw
real(prec) ,dimension(pw-1-plhgw)::uhgw

integer i




wlb=minvaut-.05*(vmaut(ns)-vmaut(1))				! w lower bound
if ((wlb<0.0).and.(sigma<1.0)) then
	wlb=.01
end if





whb=maxvaut+.3*(vmaut(ns)-vmaut(1))	     			! w upper bound
if ((whb>0.0).and.(sigma>1.0)) then
	whb=-.01
end if


wwc=.5						  ! if wc=1 then grid centered in vaut(h)	
center=wlb+(whb-wlb)/2.0 ! if wc=0 then centered  in the upper bound


ustepsize=(whb-center)/phhgw
lstepsize=(center-wlb)/plhgw


lhgw=(/(i*lstepsize,i=0,plhgw-1)/)
lhgw=wlb+lhgw					! lower half of w grid
uhgw=(/(i*ustepsize,i=1,phhgw)/)
uhgw=center+uhgw

gridw=(/ lhgw,center,uhgw /)
ngridw=gridw

 !print*,'Grid for w'
 !print "(f8.3)",gridw
 !pause



 do sc=1,ns
	do wc=1,pw
		if (gridw(wc)>vmaut(sc)) then
			gridw(wc)=vmaut(sc)
			exit
		endif
	end do
end do






 print*,' Grid Upper bound ',whb
 print*,' Grid Lower bound ',wlb
 print*,' Grid Center ',center
 print*,'Minimum Value of Autarky (No Saving Allowed) ',minvaut
 print*,'Maximum Value of Autarky (Saving High R) ',maxvaut
 pause




end subroutine dgrid



  