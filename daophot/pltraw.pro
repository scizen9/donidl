pro pltraw,ifile,off,sigma,maglim=maglim
;
; plot daophot *.raw file, first two mags against each other
;
if n_params(0) lt 1 then begin
	print,'Usage: pltraw, rawfile, magoff, sigma [, maglim=maglim]'
	return
endif
;
if keyword_set(maglim) then $
	mlim = maglim $
else	mlim = 50.0
;
openr,1,ifile
;
filt = strmid(ifile,0,1)
cfilt = strupcase(filt)
;
rec = ''
for i=0,2 do readf,1,rec
;
m1 = 0. & e1 = 0. & m2 = 0. & e2 = 0.
;
mag1 = [m1] & err1 = [e1] & mag2 = [m2] & err2 = [e2]
;
n = 0L
;
while not eof(1) do begin
	readf,1,m1,e1,m2,e2, format='(25x,f9.4,f9.4,f9.4,f9.4)'
	mag1 = [mag1,m1]
	err1 = [err1,e1]
	mag2 = [mag2,m2]
	err2 = [err2,e2]
	n = n + 1
endwhile
close,1
;
mag1 = mag1(1:*)
err1 = err1(1:*)
mag2 = mag2(1:*)
err2 = err2(1:*)
;
good = where(mag1 lt mlim)
mag1 = mag1(good)
err1 = err1(good)
mag2 = mag2(good)
err2 = err2(good)
;
good = where(mag2 lt 90.)
mag1 = mag1(good)
err1 = err1(good)
mag2 = mag2(good)
err2 = err2(good)
;
diff = mag1 - mag2
ims,diff,off,sigma
;
xrng=[max(mag1)+1.,min(mag1)-1.]
;xrng=[min([(mlim+1),25.0]), 11]
yrng=[off-2.5,off+2.5]
;
;
plot,mag1,diff,psym=4,thick=3,charsi=2,charthi=3,xtitle=cfilt+' mag', $
	xthick=3,ythick=3, title=ifile,ytitle='('+cfilt+'-'+filt+') mag', $
	xran=xrng,yran=yrng,xsty=1,ysty=1
oploterr,mag1,diff,err2,3
oplot,xrng,[off,off]
oplot,xrng,[off+sigma,off+sigma],linesty=2
oplot,xrng,[off-sigma,off-sigma],linesty=2
;
xl = xrng(0) - (xrng(0)-xrng(1)) * 0.1
yl = yrng(0) + (yrng(1)-yrng(0)) * 0.1
xyouts,xl,yl,'Mean: '+string(off,form='(f6.3)')+' +- '+ $
	string(sigma,form='(f6.3)'),charsi=2,charthi=3
;
return
end	; pro pltraw
