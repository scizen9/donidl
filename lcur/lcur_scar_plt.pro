pro lcur_scar_plt
;
lcur_scar_run
if ( lcur_com_get('nif') le 0 ) then begin
	print,'LCUR_SCAR_PLT: ERROR, no data to plot'
	return
endif
;
inu	= lcur_com_get('nu')
ipx	= lcur_com_get('px')
ipd	= lcur_com_get('pd')
npks	= lcur_com_get('npks')
pkis	= lcur_com_get('pkis')
signi	= lcur_com_get('signi')
def_fran= lcur_com_get('def_fran')
fran	= lcur_com_get('fran')
;
;!p.multi=[0,1,2]
if def_fran then $
	xr = [min(inu),max(inu)] $
else	xr = fran
plot,inu,ipx,thick=1,charsize=1.5,ytitle="!5P!DN!N(!7x!X)", xran=xr,$
	xtitle="!7x !5(1/d)", xsty=1, $
	title='!5'+systime(0),xthick=2.0, ythick=2.0,charthick=1.5,font=-1
x0 = !x.crange(0) + (!x.crange(1) - !x.crange(0) ) / 30
yoff = (!y.crange(1) - !y.crange(0) ) / 20
oplot,xr,[signi,signi],linesty=3
xyouts,x0,signi+yoff,'99%',charsize=1.5
xyouts,x0,!y.crange(1) - (yoff*2),'Pks->',charsize=1.5
y0 = !y.crange(1) - (!y.crange(1) - !y.crange(0) ) / 10
y1 = !y.crange(1)
for i=0,npks-1 do begin
	oplot,[inu(pkis(i)),inu(pkis(i))],[y0,y1]
	xyouts,inu(pkis(i)),y0-(y1-y0)*0.3,strn(i+1)
endfor
;
; plot power versus period
;
;plot,ipd,ipx,thick=1,charsize=1.5,xtitle='!5P (d)',ytitle='!5P!DN!N(P)',  $
;	xthick=2.0, ythick=2.0,charthick=1.5,font=-1,xsty=1,/xlog
;x0 = !x.crange(0) + (!x.crange(1) - !x.crange(0) ) * 0.85
;x0 = 10.0^x0
;yoff = (!y.crange(1) - !y.crange(0) ) / 20
;oplot,[min(ipd),max(ipd)],[signi,signi],linesty=3
;xyouts,x0,signi+yoff,'99%',charsize=1.5
;xyouts,x0,!y.crange(1) - (yoff*3),'<-Pks',charsize=1.5
;y0 = !y.crange(1) - (!y.crange(1) - !y.crange(0) ) / 4
;y1 = !y.crange(1)
;for i=0,npks-1 do $
;	oplot,[ipd(pkis(i)),ipd(pkis(i))],[y0,y1]
;!p.multi=0
;
return
end	; pro lcur_scar_plt
