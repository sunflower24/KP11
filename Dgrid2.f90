subroutine dgrid2  ! Set the grid for the enforcement and exogenous incomplete mkts
use params

implicit none
real(prec) :: clb,chb,cstepsize,blb,bhb,bstepsize
real(prec) :: agconst=40.0    ! Constant for exog. incomplete grid
real(prec) :: mgconst=33.0 !mgconst=1.0/(1.0-beta) ! 33.0    ! Constant for autarky grid
integer i 
 
 
 
 ! set grid for consumption

 clb=0.0005
 chb=1.5*yat(ns)
 cstepsize=(chb-clb)/(pc-1)
 gridc=(/(i*cstepsize,i=0,pc-1)/)
 gridc=gridc+clb



 ! set grid for uncontingent bonds


 blb=blimit
 do rc=2,pa
	grida(rc)=agconst*yat(ns)*((real(rc)/real(pa)))**(sigma)
 end do
 

 grida=grida+blb




 ! set grid for savings in autarky (same spacing as uncontingent bonds grid) 

gridm(1)=0.0
do rc=2,pm
	gridm(rc)=mgconst*((real(rc)/real(pm)))**(1.0+sigma/8.0)
end do

 
end subroutine dgrid2