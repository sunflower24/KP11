subroutine cgini(grid,pdf,gindex)
use params
implicit none

integer i
real(prec),dimension(pc,ns) :: pdf
real(prec),dimension(pc) :: cdf,lorx,lory,grid,pdf2
real(prec) gindex,mass,cons,tcon


! first compute the CDF

cdf(1)=sum(pdf(1,1:ns))
do i=2,pc
	pdf2(i)=sum(pdf(i,1:ns))
	cdf(i)=cdf(i-1)+pdf2(i)
	
end do

tcon=sum(pdf2*grid)

! compute the lorenz curve

do i=1,pc
	mass=sum(pdf2(1:i))
	cons=sum(grid(1:i)*pdf2(1:i))/tcon
	lory(i)=cons
	lorx(i)=mass

	if (mass>.999) then
		exit
	end if

end do


gindex=sum(lorx-lory)/sum(lorx)
print*,'The gini index is'
print*,gindex
pause 


end subroutine cgini