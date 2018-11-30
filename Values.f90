subroutine values		   ! computes value functions

use params
implicit none

real(prec),dimension(pw*ns) :: A,svfun
real(prec),dimension(pw*ns,pw*ns)::B,eyeB,IB,IBI
real(prec) vals(2)
integer ssc,inds(2)

eyeB=0.0
B=0.0
A=0.0
IB=0.0
IBI=0.0
svfun=0.0


do sc=1,ns
	do wc=1,pw

		A((sc-1)*pw+wc)=Cfun(sc,wc)*(R-1.0)/R
		eyeB((sc-1)*pw+wc,(sc-1)*pw+wc)=1.0

		do ssc=1,ns

			call basefun(gridw,pw,gfun(sc,wc,ssc),vals,inds)


			B((sc-1)*pw+wc,inds(1)+(ssc-1)*pw)=vals(1)*probs(sc,ssc)
			B((sc-1)*pw+wc,inds(2)+(ssc-1)*pw)=vals(2)*probs(sc,ssc)


		end do
	end do
end do





B=B/R
IB=eyeB-B


call dlinrg(pw*ns, IB, pw*ns, IBI, pw*ns)
svfun=matmul(IBI,A)

do sc=1,ns
	vfun(sc,1:pw)=svfun((sc-1)*pw+1:sc*pw)
end do



end subroutine values



