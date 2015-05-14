pro pltdaoap,apfile,flim,cal,siglim=siglim
;
if n_params(0) lt 1 then begin
	print,'PLDAOAP: Usage - pltdaoap, apfile, frame_limit, calib_offset'
	return
endif

callb=''
if n_params(0) ge 2 then $
	callb='('+string(cal,form='(f6.3)')+')' $
else	cal=0.
if n_elements(siglim) gt 0 then $
	sigl=siglim $
else	sigl=5.0

rddaoap,apfile,id,x,y,mags,merrs,sky,skysig
mags=mags+cal
flxs=10.0^(0.4*(25.0+mags))
;
!p.multi=[0,2,2]
;
good=where(mags lt 50. and merrs lt 2.)

plot,sky,skysig,psym=3,charthi=3,xtitle='SKY',ytitle='!7r!3!DSKY!N', $
	xthick=3,ythick=3,xcharsi=1.2,ycharsi=1.2,charsi=1.2,title=apfile

smg= ( 25.0 - 2.5 * alog10( sigl*skysig ) ) + cal
sflx=10.0^(0.4*(25.0+smg))
rat=mags/smg
;rat=flxs/sflx
getlims,mags(good),xran,xdel,rat(good),yran,ydel,fu1=0.1,fd1=0.1,fu2=0.05,$
	fd2=0.05
plot,mags(good),rat(good),psym=3,yran=yran,charthi=3,xtitle='AP MAG'+callb, $
	ytitle='AP MAG / MAG('+string(sigl,form='(f4.1)')+'*!7r!3!DSKY!N)', $
	xsty=1,ysty=1, xthick=3,ythick=3,xcharsi=1.2,ycharsi=1.2,charsi=1.2, $
	xran=xran
oplot,[-100,100],[1.0,1.0],thick=2
y=rat(good)
x=mags(good)
t=sort(x)
x=x(t)
y=y(t)
c=poly_fit(x,y,1,yfit=yfit,sig=sig,yerror=yerr)
print,'Coefs: ',c,form='(a,2f8.3)'
print,'Sigs : ',sig,form='(a,2f8.3)'
print,'Yerr : ',yerr,form='(a,f8.3)'
oplot,x,yfit
flim=(1.-c(0))/c(1)
oplot,[flim,flim],[-100,100]
xyouts,xran(0)+xdel*0.1,yran(1)-ydel*0.1,charsi=1.2,charthi=2, $
	'LIM ='+string(flim,form='(f6.2)')

yran=[-0.1,2.0]
plot,mags,merrs,psym=3,xran=xran,yran=yran,xsty=1,ysty=1,charthi=3, $
	xtitle='AP MAG'+callb,ytitle='AP ERR',xthick=3,ythick=3, $
	xcharsi=1.2,ycharsi=1.2,charsi=1.2
oplot,[flim,flim],[-100,100]

;yran=[-0.099,2.0]
;xran=[14,18]
;plot,smg,merrs,psym=3,xran=xran,yran=yran,xsty=1,ysty=1,charthi=3, $
;	ytitle='AP ERR',xtitle='MAG(10*!7r!3!DSKY!N)', xthick=3,ythick=3, $
;	xcharsi=1.2,ycharsi=1.2,charsi=1.2
;oplot,[-100,100],[-100,100]
yran=[-0.009,0.2]
plot,mags,merrs,psym=3,xran=xran,yran=yran,xsty=1,ysty=1,charthi=3, $
	xtitle='AP MAG'+callb,ytitle='AP ERR', xthick=3,ythick=3, $
	xcharsi=1.2,ycharsi=1.2,charsi=1.2
oplot,[flim,flim],[-100,100]
oplot,[-100,100],[0.01,0.01],linesty=2

!p.multi=0

return
end
