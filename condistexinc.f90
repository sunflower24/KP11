subroutine condistexinc		! Compute the stationary distribution of consumption


use params
implicit none
integer inds(2),scc
real(prec),dimension(2)::vals
real(prec) mass,testc,exput1,exput2

	open(unit=26,file='cidist.txt')


cidist=0.0
do sc=1,ns
	do ac=1,pa
			 
			 mass=aomega(sc,ac)
			 call basefun(gridc,pc,cifun(sc,ac),vals,inds)

			 if ( (minval(vals)<0.0) .and. ( mass>0.00001 ) ) then
				print*,'Enlarge your consumption grid'
				print*,'Consumption is ',cifun(sc,ac)
			 end if

			 cidist(inds(1),sc)=cidist(inds(1),sc)+vals(1)*mass
			 cidist(inds(2),sc)=cidist(inds(2),sc)+vals(2)*mass


		
	end do
end do

exput1=0.0
do sc=1,pc
	do scc=1,ns

	exput1=exput1+U(gridc(sc))*cidist(sc,scc)

	end do
end do



testc=sum(cidist)
print*,'Test C (this should be 1) ',testc
print*,'Expected Utility 1 is ',exput1



! Computing standard deviation of log consumption
avglc=0.0
do sc=1,ns
	avglc=avglc+sum(log(gridc)*cidist(1:pc,sc))
end do

stdlc=0.0
do sc=1,ns
	stdlc=stdlc+sum(((log(gridc)-avglc)**2)*cidist(1:pc,sc))
end do
stdlc=sqrt(stdlc)





rewind(26)
write(26,fmt=*) cidist,gridc  
print*,' Cons. Distr. saved '
rewind(26)
!pause



end subroutine condistexinc

