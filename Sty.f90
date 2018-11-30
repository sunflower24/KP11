 subroutine sty

 use params
 implicit none

 integer i,j,k,l


 ! tranform the states

 do i=1,ns
   
   states(i)=mstates(i)

 end do


 ! tranform the stationary distribution

 do i=1,ns
	
   stae(i)=0.0
   do j=1,ns
		stae(i)=stae(i)+mstae(i+(j-1)*ns)
   end do

 end do


 ! transform the markov transition matrix

 do i=1,ns
	do j=1,ns
		probs(i,j)=0.0
		do k=1,ns
			do l=1,ns
				probs(i,j)=probs(i,j)+ ( mprobs(i+(k-1)*ns,j+(l-1)*ns) * mstae(i+(k-1)*ns) )/stae(i)
			end do
		end do
	end do
 end do



 end subroutine sty

