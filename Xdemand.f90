subroutine xdemand			! This subroutine stores the excess demand function
use params
implicit none

integer,parameter ::npo=10			! number of points for R 
real(prec),dimension(npo)::Rvec,xdv 	! R vector and xdemand vector
real(prec) :: intv ! spacing between the R
real(prec) :: inpoint,finpoint
integer i

open(unit=18,file='xdem.txt')
inpoint=0.99  ! initial R for which we compute xcess demand
finpoint=1.05 ! final R 
intv=(finpoint-inpoint)/(npo-1)
rvec=(/(i*intv,i=0,npo-1)/)
rvec=rvec+inpoint
print*,rvec
pause

do i=1,npo
call resid(rvec(i),xdv(i))
end do
print*,'xdemand is'
print*,xdv

write(18,fmt=*)rvec,xdv 
close(18)

end subroutine xdemand

 


