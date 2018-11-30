! This subroutine computes the value of autarky when agents are
! allowed to save after being banned from trade

subroutine autvalsav

use params
implicit none


integer,parameter::maxiter=1000
integer::iter
real(prec)::minm,maxmn,gue,sdif,cmdif,vpmdif,sol(1),vcm
real(prec),dimension(pm*ns) :: A,svfun
real(prec) vals(2)
integer ssc,inds(2)
real(prec),dimension(pm*ns,pm*ns) :: BBB,eyeBBB



external syssav


Rm=R          ! Set the interest rate 


! Initial guesses


	

	do rc=1,pm
		do sc=1,ns

			cmfun(sc,rc)=(Rm-1.0)*gridm(rc)+yat(sc)
			vpmfun(sc,rc)=Rm*MU(cmfun(sc,rc))
			sfun(sc,rc)=yat(sc)+Rm*gridm(rc)-cmfun(sc,rc)
			
		end do
	end do


do iter=1,maxiter

!	print*,'Iteration number ',iter

	sfunn=0.0
	cmfunn=0.0
	vpmfunn=0.0
	

    do sc=1,ns		
       	do rc=1,pm

		 gue=sfun(sc,rc)
       	 call dneqnf(syssav,errel,1,itmax,gue,sol,fnorm)

		 if (sol(1) < 0.0 ) then
			sol(1) = 0.0
		 end if


		 sfunn(sc,rc)=sol(1)
		 cmfunn(sc,rc)=Rm*gridm(rc)+yat(sc)-sfunn(sc,rc)
		 vpmfunn(sc,rc)=MU(cmfunn(sc,rc))*Rm


				

       	
       end do
    end do
       
    
       
    sdif=100*maxval( abs( (sfunn-sfun) ) )
 	cmdif=100*maxval( abs( (cmfunn-cmfun)/cmfun ) )
	vpmdif=maxval( abs( (vpmfunn-vpmfun) ) )



    !print*," hdif ",hdif
	!print*," gdif ",gdif 
	!print*," mudif",mudif
    
    cmfun=cmfunn
	sfun=sfunn
	vpmfun=vpmfunn





!	print*,max(sdif,cmdif,vpmdif)


    
    
    if ( max(sdif,cmdif,vpmdif) < tol*0.01 ) then
    exit
    endif
       
       
end do

print*,'Convergence for Autarky Achieved after '
print*,iter
print*,'iterations'



!do rc=1,pm
!	print*,cmfun(1:ns,rc)
!	print*,sfun(1:ns,rc)
!	pause
!end do





open(unit=25,file='sfun.txt')
rewind(25)
write(25,fmt=*) sfun,gridm  
print*,' Saving Functions saved '
rewind(25)

! Compute value function



A=0.0
BBB=0.0
svfun=0.0


do sc=1,ns
	do rc=1,pm

		A((sc-1)*pm+rc)=(1.0-beta)*U(cmfun(sc,rc))
		eyeBBB((sc-1)*pm+rc,(sc-1)*pm+rc)=1.0

		do ssc=1,ns

			call basefun(gridm,pm,sfun(sc,rc),vals,inds)


			BBB((sc-1)*pm+rc,inds(1)+(ssc-1)*pm)=vals(1)*probs(sc,ssc)
			BBB((sc-1)*pm+rc,inds(2)+(ssc-1)*pm)=vals(2)*probs(sc,ssc)


		end do
	end do
end do





BBB=beta*BBB
BBB=eyeBBB-BBB


call dlinrg(pm*ns, BBB, pm*ns, BBB, pm*ns)
svfun=matmul(BBB,A)

do sc=1,ns
	vmfun(sc,1:pm)=svfun((sc-1)*pm+1:sc*pm)
end do

do sc=1,ns
	vmaut(sc)=vmfun(sc,1)
end do

open(unit=26,file='vmfun.txt')
rewind(26)
write(26,fmt=*) vmfun,gridm  
print*,' Value Functions saved '
rewind(26)

open(unit=12,file='autval.txt')

rewind(12)
write(12,fmt=*) vmaut,evaut
rewind(12)







end subroutine autvalsav