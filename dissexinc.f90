subroutine dissexinc

 use params




implicit none


integer inds(2),i
integer unevalc,uneval(ns*pa)
real(prec), dimension(ns*pa,ns*pa)::Amat,revec 
real(prec) :: vals(2),sdtol=0.0001,reval(ns*pa)
complex(8),dimension(ns*pa,ns*pa):: Evec
complex(8),dimension(ns*pa) :: Eval
integer scc

	open(unit=23,file='dissexinc.txt')

Amat=0.0

do sc=1,ns
	do ac=1,pa
		do scc=1,ns
		
		call basefun(grida,pa,aifun(sc,ac),vals,inds)

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




		AMat((scc-1)*pa+inds(1),(sc-1)*pa+ac)=vals(1)*probs(sc,scc)
		AMat((scc-1)*pa+inds(2),(sc-1)*pa+ac)=vals(2)*probs(sc,scc)

		end do



	end do
end do

print*,sum(Amat(1:ns*pa,1))




CALL DEVCRG (ns*pa, Amat, ns*pa, EVAL, EVEC, ns*pa)

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

adistr(1:pa*ns)=revec(1:pa*ns,uneval(1))/sum(revec(1:pa*ns,uneval(1)))



rewind(23)

do sc=1,ns
	write(23,fmt=*) adistr((sc-1)*pa+1:sc*pa)
end do 
	write(23,fmt=*)	grida

print*,' Distr. saved '
print*,' This should be one ',sum(adistr)
rewind(23)





end if


print*,'Number of unit Eigen-values ',unevalc
end subroutine dissexinc