! programs for tax paper revision, november 2007


program main

use params
implicit none
real(prec) sol,vcm
integer,parameter:: nbetas=2
real(prec),dimension(nbetas):: betavec=(/.95,.7/) !betavec=(/.95,.95,.9,.8,.7/)

integer ini,i

external resid

call erset(4,-1,0)          !Change stop-setting for fatal errors
							!if the 3rd element =0 then do not stop 
							!if the 3rd element =1 then stop





! parametrize markov chains	and tax policy
call mchain2



! betavec contains all the values for beta that we use in the picture
! make sure that the first value is low enough so that the economy is not in 
! complete markets


do i=1,nbetas

beta=betavec(i)
call autval           ! used to compute minimum value of autarky
vcm=U(sum(stae*yat))  ! Compute value in complete markets


if ( maxval(vmaut) <= vcm ) then
	print*, ' Complete markets is an equilibrium!!!!'
	print*, ' Max valaut ',maxval(vmaut)
	print*, ' Value cm   ',vcm
	pause
endif
 


     
call dgrid2       


minvaut=vmaut(1) 

R=1.0/beta
call autvalsav        ! used to compute maximum value of autarky
maxvaut=vmaut(ns)




call dgrid


	R=1.01
	call guess
	call cmarkets

call xdemand
pause



	R=1.0/beta-0.01
	call guess
	call cmarkets




! Solve for R

call Newton(resid,R)


print*,"equilibrium intertemporal price "
print*,R
pause




end do



! design grids for ex incomplete, consumption and autarky with savings
call dgrid2       

call autval           ! used to compute minimum value of autarky
minvaut=vmaut(1) 

R=1.0/beta
call autvalsav        ! used to compute maximum value of autarky
maxvaut=vmaut(ns)

!call exincmarkets


												
pause



44 print*,' Enter (1) for Saving in Autarky, (2) for No Saving '
read*,fsav
vcm=U(sum(stae*yat))

select case(fsav)


case(1)

 
print*,'Value of Complete Markets ',vcm
print*,'Value of Autarky with R=1/beta= ',R
print*,vmaut


case(2)
call autval

print*,'Value of Complete Markets ',vcm
print*,'Value of Autarky with no Saving'
print*,vmaut

case default
goto 44

end select







 if ( vcm > vmaut(ns) ) then 
	print*,'v cm ',vcm
	print*,'max vaut ',vmaut(ns) 
	print*, 'Equal weights First best is sustainable !'
 end if


! Initialize intertemporal price
R=1.028


! compute unconstrained values and consumption
call guess
call cmarkets




!call iniguess

!open(unit=11,file='drules.txt')

print*,'Enter 1 for loading a guess, 2 for start from CM'
read*,ini

if ( ini==1 ) then

	call loadfun

end if

! Compute xcess demand
!call xdemand
!pause



! Solve for R

call Newton(resid,R)


print*,"equilibrium intertemporal price "
print*,R
pause



call condist
call cgini(gridc,cdist,ginico)
pause

call cmarkets

print*,"equilibrium intertemporal price "
print*,R
pause

end program main