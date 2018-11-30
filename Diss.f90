subroutine diss

 use params




implicit none


integer inds(2),i
integer unevalc,uneval(ns*pw)
real(prec), dimension(ns*pw,ns*pw)::Amat,revec 
real(prec) :: vals(2),sdtol=0.0001,reval(ns*pw)
complex(8),dimension(ns*pw,ns*pw):: Evec
complex(8),dimension(ns*pw) :: Eval
integer scc

	open(unit=13,file='diss.txt')
	open(unit=15,file='amat.txt')

Amat=0.0

do sc=1,ns
	do wc=1,pw
		do scc=1,ns


		call basefun(gridw,pw,gfun(sc,wc,scc),vals,inds)

		if (vals(1)<0.0) then
		vals(1)=0.0
		endif

		if (vals(2)>1.0) then
		vals(2)=1.0
		endif

		if (vals(2)<0.0) then
		vals(2)=0.0
		endif

		if (vals(1)>1.0) then
		vals(1)=1.0
		endif




		AMat((scc-1)*pw+inds(1),(sc-1)*pw+wc)=vals(1)*probs(sc,scc)
		AMat((scc-1)*pw+inds(2),(sc-1)*pw+wc)=vals(2)*probs(sc,scc)

		end do



	end do
end do






CALL DEVCRG (ns*pw, Amat, ns*pw, EVAL, EVEC, ns*pw)

reval=dreal(eval)
revec=dreal(evec)

print*,'Eigen-values computed '




if (reval(1)<1.0-sdtol) then
 print*,' No stationary distribution here !!'
end if



unevalc=0
do i=1,pw*ns

	if (reval(i)<1.0-sdtol)  then
		exit
	end if 

	
	if (reval(i)>1.0+sdtol) then
		cycle
	end if

	unevalc=unevalc+1
	uneval(unevalc)=i
	print*,eval(i)


  end do						

if ( unevalc==1) then 
	print*,'Unique stationary distribution '
	!print*, revec(1:pw*ns,uneval(1))

	distr(1:pw*ns)=revec(1:pw*ns,uneval(1))/sum(revec(1:pw*ns,uneval(1)))

	rewind(13)

	do sc=1,ns
		write(13,fmt=*) distr((sc-1)*pw+1:sc*pw)
	end do 
	write(13,fmt=*)	gridw

	print*,' Distr. saved '
	rewind(13)


end if

if (unevalc>1) then
	print*,'Warning...'
	print*,'No unique stationary distribution..'
	print*,'Number of unit Eigen-values ',unevalc
	pause
end if



end subroutine diss