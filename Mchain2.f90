 subroutine mchain2
 use params
 use MSIMSL

 implicit none

 
 character,dimension(15) :: taxpol
 integer ini,i

  ! endowment process parameters
  ! Heaton and Lucas or Ayiagari
 real(prec) :: rho,mmc,vmc,var
 real(prec) :: sigma2eps
 real(prec) :: toll=.00001

 complex(8),dimension(ns) :: eval
 complex(8),dimension(ns,ns) :: evec
 real(prec),dimension(ns) :: reval
 real(prec),dimension(ns,ns) :: eye,revec

 ! STY
 real(prec):: rhosty
 real(prec):: sigma2epssty,sigma2eta
 real(prec),dimension(2,2)::sigma2,sigmaf,sigmac


 real(prec),dimension(ns) :: esttax
 real(prec) acoeff,bcoeff,bhat
    	print *,"Enter 1 for STY, 2 for Ayagari, 3 for CES"
		read *,ini 
 


 if (addy>1.0) then
	print*,'ATTTENTION ATTENTION !!!!'
	print*,addy
 endif


!  STY specification


	if (ini==1)	then


		nvar=2
		nval(1:2)=ns
		nsm=ns*ns


		allocate(mstae(ns**nvar))	
		allocate(mstates(ns**nvar))
		allocate(mprobs(ns**nvar,ns**nvar)) 


		rhosty=.93
		sigma2epssty=.02
		sigma2eta=.053
		sigma2=sigma2eta
		sigma2(1,1)=sigma2epssty+sigma2eta


		theta(1:nvar)=0.0
		theta(nvar+1:nvar+nvar*nvar)=0.0
		theta(nvar+3:nvar+nvar*nvar)=rhosty
		theta(nvar+nvar*nvar+1: nvar+nvar*nvar+2)=sigma2(1,1:2)
		theta(nvar+nvar*nvar+3: nvar+nvar*nvar+4)=sigma2(2,1:2)
		


		call markov


		call sty
	end if



! Ayagari specification

	if (ini==2) then


		nvar=1
		nval(1)=ns
		nsm=ns


		allocate(mstae(ns**nvar))	
		allocate(mstates(ns**nvar))
		allocate(mprobs(ns**nvar,ns**nvar)) 


		rho=0.7        ! persistence of the AR(1)
		sigma2eps=.16  ! variance of epsilon


		theta(1)=0.0
		theta(2)=rho
		theta(3)=sigma2eps
		call markov
		stae=mstae(1:ns)
		states=mstates(1:ns)
		probs=mprobs(1:ns,1:ns)



		mmc=sum(states*stae)
		vmc=sum(stae*(states-mmc)**2)
		var=sigma2eps/(1.0-rho**2)
	    print*,'Variance of the Markov Chain is ',vmc
		print*,'Variance of the AR(1) is        ',var




	end if


   if (ini==3) then
	
	open(unit=98,file='cesmat.txt')
	read(98,fmt=*) states,probs,stae,esttax
	states=log(states)
	close(98)


   end if


   ! deallocate(mstae,mstates,mprobs)
   ! Compute unconditional percentage standard deviation

 
  
 

 states=exp(states)
 states=states/(sum(stae*states))    ! normalization of e
								     ! e are ordered e1<e2...<en


 states=states

 if ( ini==3) then
   trates=esttax
   g=sum( (trates*stae)*states )
   print*,' g is ',g
 endif


 ! Compute second eigen value of the transition matrix

 call devlrg(ns,probs,ns,eval)
 print*,'Eigen values of the transition matrix'
 reval=real(eval)
 print*,reval
 print*,'Stationary Distribution'
 print*,stae

 do i=1,ns
	eye(i,i)=1.0
 end do

 ! mixing the transition probabilities with the identity matrix
 probs=((1.0)-addpers)*probs + addpers*eye   

 if (abs(addpers)>0.0) then


	CALL devcrg(ns,probs,ns,eval,evec,ns)
	print*,'Modified persistence'
	print*,'Addpers = ',addpers
	print*,'Eigen values of the new transition matrix'
	reval=real(eval)
	print*,reval
	print*,' '
	pause

 endif








100 print*,'Lump sum(1), Proportional(2),  Progressive (3), Experiments (4) ?'
    read*,porl

 select case(porl)

 
 case(1)
	! lump sum

	 print*,'Lump sum taxes'
	 taxpol="Lump sum   "   
	 trates=g/states
	 print*,'Public spending ',g

 case(2)
	! proportional

	 print*,'Proportional '
	 print*,'Public spending is ',g
	 trates=g



 case(3)
	! progressive

	 print*,'Progressive '
	 trates=esttax
	 g=sum( (trates*stae)*states )
	 print*,'Public spending is ',g

 case(4) !experiments
     g=.1554
	 bhat=0.0961
	 bcoeff=2.0*bhat
     acoeff=g+bcoeff
	 trates=acoeff-bcoeff/states	



 case default
	goto 100


 end select


 print*,'Before tax incomes'
 print*,states
 	print*,'Unconditional Standard dev of log of before tax earnings is '
	stdle=sqrt(sum( stae* (log(states) - sum(stae*log(states)))**2 ))
	print*,stdle


 yat=states-trates*states ! after tax income

 yat=yat*addy


 print*,'After tax incomes'
 print*,yat
  	print*,'Unconditional Standard dev of log of after tax earnings is '
	stdly=sqrt(sum( stae* (log(yat) - sum(stae*log(yat)))**2 ))
	print*,stdly
 pause
 print*,'Transition probabilities '
 CALL dWRRRN ('Transition probabilities', ns, ns, probs, ns,0)




 print*,'Stationary distribution'
 print*,stae

 print*,'Tax rates '
 print*,trates

 pause 
 
 end subroutine mchain2
