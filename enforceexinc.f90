
subroutine enforceexinc

    use params
	implicit none

	integer :: iter,maxiter=10000
	real(prec),dimension(1) :: sol,gue
	real(prec) aidif,cidif,vpdif
    
   
	external sysex

	open(unit=21,file='arules.txt')





print*,'Ri is ',Ri


do iter=1,maxiter

!	print*,'Iteration number ',iter

	aifunn=0.0
	cifunn=0.0
	vpfunn=0.0
	

    do sc=1,ns		
       	do ac=1,pa

		 gue=aifun(sc,ac)
       	 call dneqnf(sysex,errel,1,itmax,gue,sol,fnorm)

		 if (sol(1) < blimit ) then
			sol(1) = blimit
		 end if


		 aifunn(sc,ac)=sol(1)
		 cifunn(sc,ac)=Ri*grida(ac)+yat(sc)-aifunn(sc,ac)
		 vpfunn(sc,ac)=MU(cifunn(sc,ac))*Ri


				

       	
       end do
    end do
       
    
       
    aidif=100*maxval( abs( (aifunn-aifun) ) )
 	cidif=100*maxval( abs( (cifunn-cifun)/cifun ) )
	vpdif=maxval( abs( (vpfunn-vpfun) ) )



    !print*," hdif ",hdif
	!print*," gdif ",gdif 
	!print*," mudif",mudif
    
    cifun=cifunn
	aifun=aifunn
	vpfun=vpfunn





!	print*,max(aidif,cidif,vpdif)


    
    
    if ( max(aidif,cidif,vpdif) < tol*0.01 ) then
    exit
    endif
       
       
end do

print*,'convergence achieved after '
print*,iter
print*,'iterations'


rewind(21)
write(21,fmt='(f6.3)') cifun
rewind(21)
pause







end subroutine enforceexinc

