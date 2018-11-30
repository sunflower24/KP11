subroutine Newton(fun,gues)

use params

implicit none

integer :: i, maxit=1000
real(prec) ::eps=0.0000001,delta, gues,guesp,ngues,fval1,fval2


do i=1,maxit
	print*,'Guess is ',gues

	if (gues==0.0) then
    	call fun(gues,fval2)

		if (abs(fval2)<tol) then
			exit
		end if

		call fun(gues+eps,fval1)

	delta=( fval1-fval2 )/( eps )

	else
	call fun(gues,fval2)

	if (abs(fval2)<tol*10.0) then
		exit
	end if


	guesp=gues+gues*eps

	call fun(guesp,fval1)



	delta=( fval1-fval2 )/( gues*eps )
	if (abs(delta)<0.0005) then
		delta=0.0005*abs(delta)/delta
	end if
	print*,'delta is',delta


end if
	
ngues=gues-fval2/delta

if (ngues>1.0/beta) then
	ngues=1.0/beta-0.01
end if


if (ngues<(1.0/beta)*(yat(1)/yat(ns))**sigma) then
	ngues=(1.01/beta)*(yat(1)/yat(ns))**sigma
end if



print*,'old gues was ',gues
print*,'Nguess is ',ngues
pause



gues=ngues
end do


end subroutine Newton