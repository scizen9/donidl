pro lcur_plt_cnts
;
if ( lcur_com_get('npts') le 0 ) then begin
	print,'LCUR_PLT_CNTS: ERROR, no data read in yet'
	return
endif
;
itm = lcur_com_twin('time')
ict = lcur_com_twin('counts')
ice = lcur_com_twin('cerrs')
ime = lcur_com_twin('merrs')
erl = lcur_com_get('merrlim')

do_sub = lcur_com_get('do_sub')
if do_sub then begin
	coffs = lcur_com_twin('coffs')
	ict = ict + coffs
endif
;
; get good data
good = where(ime le erl, ngood)
if ngood le 10 then begin
	print,'LCUR_PLT_CNTS: ERROR, error limit too strict; only ', $
		strn(ngood),' points left.'
	return
endif
itm = itm(good)
ict = ict(good)
ice = ice(good)
;
; plot data points
;
vmean=mean(ict)
vsig = stddev(ict)
ymxv = max(ict+ice) + 0.5
ymnv = min(ict-ice) - 0.5
vran = ymxv - ymnv
ylimits = [ymnv,ymxv]
;
; x limits
delx = (max(itm) - min(itm))/ 10.0
xlimits = [min(itm)-delx, max(itm)+delx] - lcur_com_get('jdzero')
;
tstr = 'ADJ CNTS ' + systime(0)
;
zd = lcur_com_get('jdzero')
xlab = 'RJD - '+strtrim(zd,2)
;
ylab = 'RELATIVE COUNTS'
plot,itm-zd,ict,psym=4,yran=ylimits,xran=xlimits,xsty=1,ysty=1,$
	thick=3,charsize=2.0,ytitle=ylab,xtitle=xlab,$
	title=tstr,xthick=2.0,ythick=2.0,charthick=1.5,font=1
errplot,itm-zd,ict-ice,ict+ice
oplot,xlimits,[vmean,vmean]
oplot,xlimits,[vmean+vsig,vmean+vsig],linesty=3
oplot,xlimits,[vmean-vsig,vmean-vsig],linesty=3
xyouts,xlimits(0)+(xlimits(1)-xlimits(0))*0.1,ymnv+0.95*vran,$
	'Mean: '+string(vmean,form='(f6.2)')+ $
	' Sig: '+string(vsig,form='(f6.2)'),charthick=1.5,charsize=2.0,font=1
;
return
end
