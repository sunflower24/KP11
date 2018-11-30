		  	  	    
 module params
 implicit none

 integer,parameter :: prec=selected_real_kind(15,300)
						



 real(prec),parameter :: sigma=1.10    ! Risk aversion
 real(prec) :: beta      	! subjective discount factor
 real(prec) :: g    					! public spending
 real(prec),parameter :: minc=0.0001, minu=0.0001  ! mimimum cons and utility
 real(prec) :: ginico



 integer,parameter::ns=5	     ! Number of individual states

 real(prec) :: addy=1.0		 ! Additional income  (multiplicative)
 real(prec) :: addpers=0.0    ! Additional persistence

 integer :: NVAR,nlag=1
 integer ::nval(10)
 integer :: nsm






 real(prec) :: theta(100)		   ! 1:nvar = constant
								   ! nvar*nvar = autoregressive matrix
								   ! nvar*nvar = epsilons var cov matrix




  ! Parameters derived from Tauchen procedure (Markov)
 real(prec),allocatable :: mstae(:)			 ! Stationary distribution of e
 real(prec),allocatable :: mstates(:)		 ! Matrix of states e1,e2
 real(prec),allocatable :: mprobs(:,:)      ! Transition probabilities


  ! Parameters used in the main program (in the VAR case they are tranformed using the sty routine)
 real(prec) :: stae(ns)			 ! Stationary distribution of e
 real(prec) :: states(ns)		 ! Matrix of states e1,e2
 real(prec) :: probs(ns,ns)      ! Transition probabilities




 real(prec) :: R				 ! Intertemporal price
 real(prec) :: trates(ns)	     ! tax rates
 real(prec) :: yat(ns)			 ! after tax income
 real(prec) :: resd 

 integer,parameter :: pw=100    ! grid points for w
 integer, parameter	:: pc=100   ! grid points for c
 real(prec),dimension(pw) :: gridw,ngridw  ! grid over w
 real(prec),dimension(pw*ns) :: distr ! Stationary distribution over w and y


 real(prec) :: govint2,privint2,totint2,avglc,stdlc,stdly,stdle





 ! Exogenous incomplete parametrs
 integer,parameter :: pa=150
 real(prec),dimension(pa) :: grida
 real(prec),dimension(ns,pa)::cifun,cifunn,aifun,aifunn,vpfun,vpfunn
 integer ac
 real(prec) :: Ri				 ! Intertemporal price
 real(prec),parameter :: blimit=-7.0
 real(prec),dimension(pa*ns) :: adistr ! Stationary distribution over assets and y
 real(prec),dimension(ns,pa)::aomega
 real(prec),dimension(pc,ns)::cidist


! Parameters for computing autarky with savings
 integer,parameter :: pm=150
 real(prec),dimension(pm) :: gridm
 real(prec),dimension(ns,pm)::cmfun,cmfunn,sfun,sfunn,vpmfun,vpmfunn,vmfun
 integer rc,fsav
 real(prec) :: Rm,minvaut,maxvaut				 ! Intertemporal price



 integer sc,wc						! counters
 
 real(prec) :: errel=0.000001, fnorm	! parameters used by the non lin eq solver
 integer :: itmax=20000
 real(prec) :: tol=0.00001            ! tolerance parameter

 real(prec),dimension(ns,pw,ns)::gfun,gfunn,ugfun
 real(prec),dimension(ns,pw)::hfun,hfunn,cfun,uhfun
 real(prec),dimension(ns,pw) ::lafun,lafunn,omega,vfun
 real(prec),dimension(ns*(ns+3)+1,pw)::output
 real(prec),dimension(1,pw) :: gout

 real(prec),dimension(ns)::vmaut,evaut
 real(prec),dimension(pc,ns)::cdist
 real(prec),dimension(pc)::gridc

 integer porl

 contains

 function U(c)
 implicit none
 real(prec) U,c																		                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       

	

 if (c<0.0) then
	c=minc
 end if

 if (abs(sigma-1.0)<.0001) then
 	U=log(c)
 else
 	U=(1.0/(1.0-sigma))*c**(1.0-sigma)
 endif
  
 end function U


 function MU(c)
 implicit none
 real(prec) MU,c

 if (c<0.0) then
	c=minc
 end if
 
 if (abs(sigma-1.0)<.0001) then
 	MU=1.0/c
 else
 	MU=c**(-sigma)
 endif
   
 end function MU


 function C(u)
 implicit none
 real(prec) C,u

 
 

if ( (u*(1.0-sigma)) <= 0.0)  then  
	u=minu*(1.0-sigma)			   
 end if

 if (abs(sigma-1.0)<.0001) then
 C = exp(u)
 else
 C = ( (1.0-sigma)*u  )** ( 1.0/(1.0-sigma) )
 endif
  
 end function C															 


 function MC(u)
 implicit none
 real(prec) MC,u

 if (abs(sigma-1.0)<.0001) then
 MC = exp(u)
 else
 MC = C(u)**sigma
 endif

 end function MC



! this subroutine returns the values and the indices of the two basis
! functions that are positive on a given x in the grid_x

subroutine basefun (grid_x,npx,x,vals,inds) 
	implicit none


	real(prec),intent(in) :: x
	integer , intent(in):: npx
	real(prec), intent(in) :: grid_x (npx)
	real(prec), intent(out) ::vals(2)
	integer ,intent(out) ::inds(2)
	integer :: i,ju,jl,jm

	
	jl=1     ! 
	ju=npx   !	


	do
	if (ju-jl<=1) exit
		
	jm=(ju+jl)/2
	if (x>=grid_x(jm)) then

		jl=jm
	else
		ju=jm
	endif
	end do


	i=jl+1

		vals(2)=( x-grid_x(i-1) )/(grid_x(i)-grid_x(i-1))
		vals(1)=( grid_x(i)-x )/(grid_x(i)-grid_x(i-1))
		inds(2)=i
		inds(1)=i-1

	
end subroutine basefun











 end module params