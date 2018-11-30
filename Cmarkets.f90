
subroutine cmarkets

    use params


    
    implicit none
	integer ::maxiter=1000,iter
	real(prec) :: hdif,gdif,ladif


	real(prec),dimension(ns)::sol1,gue1


	external sysbin

	
	open(unit=10,file='drulcm.txt')


print*,'Computing complete markets d rules'
print*,'R is ',R
print*,'1/beta is ',1.0/beta
pause


    
do iter=1,maxiter

	!print*,'Iteration number ',iter
	!pause
	hfunn=0.0
	gfunn=0.0

    do sc=1,ns
       	do wc=1,pw
		!print*,sc,wc
		!print*,gridw(wc),evaut(sc)


		 gue1(1:ns)=gfun(sc,wc,1:ns)

       	 call dneqnf(sysbin,errel,ns,itmax,gue1,sol1,fnorm)

				gfunn(sc,wc,1:ns)=sol1(1:ns)
				hfunn(sc,wc)=gridw(wc)- sum( probs(sc,1:ns)*beta*sol1(1:ns) ) 
				hfunn(sc,wc)=hfunn(sc,wc)/(1.0-beta)
				lafunn(sc,wc)=((R-1.0)/(R*(1.0-beta)) )*MC(hfunn(sc,wc))


       
       end do
    end do
       


    
       
    hdif=100*maxval( abs( (hfunn-hfun)/hfun ) )
 	gdif=100*maxval( abs( (gfunn-gfun)/gfun ) )
	ladif=maxval( abs( (lafunn-lafun) ) )



    !print*," hdif ",hdif
	!print*," gdif ",gdif 

    
    hfun=hfunn
	gfun=gfunn
	lafun=lafunn

	ugfun=gfun

	

	!print*,max(gdif,hdif,ladif)
    
    
    if ( max(gdif,hdif,ladif) < tol*0.01 ) then
    exit
    endif
       
       
end do

print*,'Complete market convergence achieved after '
print*,iter
print*,'iterations'





rewind(10)
write(10,fmt=*) gfun, hfun, lafun,vfun
rewind(10)
pause



end subroutine cmarkets

