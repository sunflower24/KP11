
subroutine enforce

    use params


    
    implicit none
	integer ::maxiter=1000,iter,inds(2),neq,i
	real(prec) :: hdif,gdif,enfv(ns),ladif,npla,vals(2)


	real(prec),dimension(ns)::sol1,gue1


	external sysbin

	open(unit=11,file='drules.txt')




print*,'R is ',R


do iter=1,maxiter

!	print*,'Iteration number ',iter

	hfunn=0.0
	gfunn=0.0

    do sc=1,ns		
       	do wc=1,pw



	   do neq=ns,1,-1

			gue1(1:neq)=gfun(sc,wc,1:neq)
			call dneqnf(sysbin,errel,neq,itmax,gue1(1:neq),sol1(1:neq),fnorm)

			gfunn(sc,wc,1:neq)=sol1(1:neq)

			if ( neq<ns) then
				gfunn(sc,wc,neq+1:ns)=vmaut(neq+1:ns)
			end if 
			
			hfunn(sc,wc)=( gridw(wc)- sum( probs(sc,1:ns)*beta*gfunn(sc,wc,1:ns) ) )/(1.0-beta)
			enfv=gfunn(sc,wc,1:ns)-vmaut			
			
			if  (minval(enfv)>-.000000001 ) then 
				goto 2001	
			end if
      
       end do



	! If program gets here all constraints are binding
	gfunn(sc,wc,1:ns)=vmaut(1:ns)
	hfunn(sc,wc)=( gridw(wc)- sum( probs(sc,1:ns)*beta*gfunn(sc,wc,1:ns) ) )/(1.0-beta)


	   
	   
	   
2001	   lafunn(sc,wc)=( (R-1.0) / (R* (1.0-beta)) )*MC(hfunn(sc,wc))



	


      
       end do
    end do
       
    
       
    hdif=100*maxval( abs( (hfunn-hfun)/hfun ) )
 	gdif=100*maxval( abs( (gfunn-gfun)/gfun ) )
	ladif=maxval( abs( (lafunn-lafun) ) )

    
    hfun=hfunn
	gfun=gfunn
	lafun=lafunn


	print*,max(gdif,hdif,ladif)


    
    
    if ( max(gdif,hdif,ladif) < tol*0.01 ) then
    exit
    endif
       
       
end do

print*,'convergence achieved after '
print*,iter
print*,'iterations'

print*,'checking..'
 do sc=1,ns
	do wc=1,pw

		if (  ( gfun(sc,wc,1)-vmaut(1) ) > 0.0001 ) then
	   		call basefun (gridw,pw,gfun(sc,wc,1),vals,inds)
			npla=vals(1)*lafun(1,inds(1))+vals(2)*lafun(1,inds(2))
		
			if ( abs( (npla-beta*R*lafun(sc,wc) ) ) > 0.0001 ) then
				print*,sc,wc
				print*,gridw(wc)
				print*,vmaut(1)
				print*,' 1 is fucked..idiot..by'
				print*,(npla-beta*R*lafun(sc,wc))



				pause
			endif
		endif

		if (  ( gfun(sc,wc,2)-vmaut(2) ) > 0.0001 ) then
	   		call basefun (gridw,pw,gfun(sc,wc,2),vals,inds)
			npla=vals(1)*lafun(2,inds(1))+vals(2)*lafun(2,inds(2))
		
			if ( abs( (npla-beta*R*lafun(sc,wc) ) ) > 0.0001 ) then
				print*,gridw(wc)
				print*,vmaut(2)
				print*,' 2 is fucked too..idiot..by'
				print*,(npla-beta*R*lafun(sc,wc))

				pause
			endif
		endif







	end do
end do
print*,'finished checking '
! pause


print*,'Computing value functions'
call values

do sc=1,ns
	output((sc-1)*ns+1:ns*sc,1:pw)=transpose(gfun(sc,1:pw,1:ns))
end do

output(ns*ns+1:ns*ns+ns,1:pw)=hfun
output(ns*ns+ns+1:ns*ns+2*ns,1:pw)=lafun
output(ns*ns+2*ns+1:ns*ns+3*ns,1:pw)=vfun

do i=1,pw
	gout(1,i)=gridw(i)
end do

output(ns*ns+3*ns+1:ns*ns+3*ns+1,1:pw)=gout



rewind(11)
!write(11,fmt='(f7.3)') output
write(11,fmt=*) output
rewind(11)
!pause







end subroutine enforce

