pro pltmc,ifl
;
rdmc,ifl,rats,mats,ntr,fld,set,type
;
prob = (mats / float(ntr)) * 100.
mxprb = max(prob)
t = where(prob eq mxprb,n)
if n gt 1 then begin
    if n mod 2 eq 1 then $
    	mxrat = t(n/2) $
    else if prob(t(0)-1) lt prob(t(n-1)+1) then $
		mxrat = t(n/2) $
	else	mxrat = t(n/2-1)
endif else mxrat = t(0)
tlb = strupcase(fld) + ' '+set+type
plot,prob,xtitle='Nova Rate (yr!E-1!N)',ytitle='Probability(%)',ythick=3, $
	charsiz=2, xthick=3,charthick=3,thick=3,title=tlb, $
	xran=[-1,max(rats)+1],xsty=1,yrange=[-0.5,min([max(prob)+1,100])],ysty=1
;
oplot,[mxrat,mxrat],[0,mxprb],linesty=2
;
ssrats = findgen(max(rats)*10)/10.
prob = interpol(prob,rats,ssrats)
tprob = total(prob)
hprob = 0
sprob = prob(reverse(sort(prob)))
psprb = 0L
;
while hprob/tprob lt 0.5 do begin
	psprb = psprb + 1
	t = where(prob ge sprob(psprb),n)
	hprob = total(prob(t))
endwhile
print,strn(hprob)+'/'+strn(tprob)+' = '+string((hprob/tprob),form='(f5.3)')
;
oplot,[ssrats(t(0)),ssrats(t(n-1))],[sprob(psprb),sprob(psprb)]
up = string(ssrats(t(n-1))-mxrat,form='(f4.1)')
down = string(mxrat-ssrats(t(0)),form='(f4.1)')
;
legend,['<R> = '+strn(mxrat)+'!S!E+'+up+'!R!I-'+down+'!N yr!E-1!N', $
	strn(ntr)+' Trials'],box=0,charsi=2.5,charthi=3,spacing=3
;
print,'<R> = '+strn(mxrat)+' +'+up+' -'+down
;
return
end	; pro nova_mc
