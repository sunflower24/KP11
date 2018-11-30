subroutine condist		! Compute the stationary distribution of consumption


use params
implicit none
integer inds(2),scc,cc
real(prec),dimension(2)::vals
real(prec) mass,testc,exput1,exput2

	open(unit=16,file='cdist.txt')


cdist=0.0
do sc=1,ns
	do wc=1,pw
			 
			 mass=omega(sc,wc)
			 call basefun(gridc,pc,cfun(sc,wc),vals,inds)

			 if ( (minval(vals)<0.0) .and. ( mass>0.00001 ) ) then
				print*,'Enlarge your consumption grid'
				print*,'Consumption is ',cfun(sc,wc)
			 end if

			 cdist(inds(1),sc)=cdist(inds(1),sc)+vals(1)*mass
			 cdist(inds(2),sc)=cdist(inds(2),sc)+vals(2)*mass


		
	end do
end do

exput1=0.0
do cc=1,pc
	do scc=1,ns

	exput1=exput1+U(gridc(cc))*cdist(cc,scc)

	end do
end do

exput2=0.0
do sc=1,ns
	do wc=1,pw
			 
		exput2=exput2+gridw(wc)*omega(sc,wc)


	end do
end do








testc=sum(cdist)
print*,'Test C (this should be 1) ',testc
print*,'Expected Utility 1 is ',exput1
print*,'Expected Utility 2 is ',exput2




rewind(16)
write(16,fmt=*) cdist,gridc  
print*,' Cons. Distr. saved '
rewind(16)

! Computing standard deviation of log consumption
avglc=0.0
do sc=1,ns
	avglc=avglc+sum(log(gridc)*cdist(1:pc,sc))
end do

stdlc=0.0
do sc=1,ns
	stdlc=stdlc+sum(((log(gridc)-avglc)**2)*cdist(1:pc,sc))
end do
stdlc=sqrt(stdlc)





end subroutine condist

