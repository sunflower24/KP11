 subroutine autval

 use params

 implicit none
 real(prec),dimension(ns,ns)::eye,mat,matinv
 real(prec),dimension(ns)::put
 integer i

 eye=0.0
 do i=1,ns
	put(i)=(1.0-beta)*U( yat(i) )
	eye(i,i)=1.0
 end do


 mat=(eye-beta*probs)
 call dlinrg(ns,mat,ns,matinv,ns)

 vmaut=matmul(matinv,put)



 evaut=matmul(probs,vmaut)
 


 end subroutine autval

 