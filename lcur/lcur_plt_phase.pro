pro lcur_plt_phase,fit=fit,fig=fig,finder=finder,mags=mags
;
if ( lcur_com_get('ntpts') le 0 ) then begin
	print,'LCUR_PLT_PHASE: ERROR, no data read in yet'
	return
endif
;
phase	= lcur_com_twin('phase')
phep	= lcur_com_twin('epnum')
icnts	= lcur_com_twin('counts')
icers	= lcur_com_twin('cerrs')
img	= lcur_com_twin('mags')
imers	= lcur_com_twin('merrs')
per	= lcur_com_get('period')
erl	= lcur_com_get('merrlim')
filt	= strupcase( lcur_com_get('filt') )
twin	= lcur_com_get('twin')
do_sub	= lcur_com_get('do_sub')
dao_cid = lcur_com_get('dao_cid')
if do_sub then begin
	coffs = lcur_com_twin('coffs')
	icnts = icnts + coffs
endif
;
; get good points
good = where(imers le erl, ngood)
if ngood le 10 then begin
	print,'LCUR_PLT_PHOT: ERROR, error limit too strict; only ', $
		strn(ngood), ' points left.'
	return
endif

phase	= phase(good)
phep	= phep(good)
icnts	= icnts(good)
icers	= icers(good)
img	= img(good)
imers	= imers(good)

if keyword_set(mags) then begin
	icnts = img
	icers = imers
endif

do_bin	= lcur_com_get('do_bin')
do_weight = lcur_com_get('do_weight')

if do_bin then begin
	bins 		= lcur_com_get('bins')
	if do_weight then $
		phasebin,phase,icnts,icers,bins,/weight $
	else	phasebin,phase,icnts,icers,bins
	good	= where(icnts gt 0., ngood)
	if ngood gt 0 then begin
		phase	= phase(good)
		icnts	= icnts(good)
		icers	= icers(good)
	endif else begin
		print,'LCUR_PLT_PHASE: Error, no good points'
		return
	endelse
endif	else begin
	phnum = phep
	phnum = phnum(sort(phnum))
	phnum = phnum(uniq(phnum))
	nphep = n_elements(phnum)
endelse

;
; plot data points
;
vmean=mean(icnts)
vsig = stddev(icnts)
if keyword_set(mags) then begin
	ymxv = max(icnts+icers) + 2.*max(icers)
	ymnv = min(icnts-icers) - 2.*max(icers)
	vran = ymxv - ymnv
	ylimits = [ymxv,ymnv]
	blab = ' MAG'
endif else begin
	ymxv = max(icnts+icers) + 15
	ymnv = min(icnts-icers) - 15
	vran = ymxv - ymnv
	ylimits = [ymnv,ymxv]
	blab = ' COUNTS'
endelse
;
if keyword_set(fig) then $
	tstr = '' $
else	tstr = 'ID: '+strn(dao_cid)+' '+lcur_com_get('pfile')+'  '+systime(0)
;
if do_bin then $
	xlab = 'BINNED PHASE ('+strn(bins)+')' $
else	xlab = 'PHASE'
;
if do_weight then $
	ylab = filt + ' WEIGHTED AVG'+blab $
else	if do_bin then $
	ylab = filt + ' AVG'+blab $
else	ylab = filt + blab

pfile	= lcur_com_get('pfile')
root	= gettok(pfile,'.')

a=findgen(17) + (!pi*2/16.)
usersym,cos(a),sin(a),/fill

if keyword_set(finder) then $
	plot,phase,icnts,psym=8,yran=ylimits,xran=[0,2],xsty=1,ysty=1,$
        	thick=3,charsize=2.0,ytitle=ylab,symsize=0.5, $
		xthick=3.0,ythick=3.0,charthick=3.0,font=1, $
		/noerase,position=[0.05,0.05,0.99,0.5] $
else plot,phase,icnts,psym=8,yran=ylimits,xran=[0,2],xsty=1,ysty=1,$
        	thick=3,charsize=2.0,ytitle=ylab,xtitle=xlab,symsize=0.5, $
		title=tstr,xthick=3.0,ythick=3.0,charthick=3.0,font=1

oplot,phase+1.0,icnts,psym=8,thick=3,symsize=0.5
errplot,phase,icnts-icers,icnts+icers
errplot,phase+1.0,icnts-icers,icnts+icers

if not do_bin then begin
	psarr = [4, 5, 2, 6, 1, 2 ]	; psym value array
	for i=0,nphep-1 do begin
		t=where(phep eq phnum(i), nt)
		if nt gt 0 then begin
			oplot,phase(t), icnts(t), psym=psarr(phnum(i)<5), $
				thick=3, symsize=1.0
			oplot,phase(t)+1.0, icnts(t), psym=psarr(phnum(i)<5), $
				thick=3, symsize=1.0
		endif
	endfor
endif

if keyword_set(fig) then begin
	xyouts,0.1,ymnv+0.90*vran, 'P(h) =' + string(per*24.0,form='(f7.3)'), $
	charthick=1.5,charsize=2.0,font=1
endif else begin

oplot,[0.0,2.0],[vmean,vmean]
oplot,[0.0,2.0],[vmean+vsig,vmean+vsig],linesty=3
oplot,[0.0,2.0],[vmean-vsig,vmean-vsig],linesty=3

yl=!y.crange(0)+(!y.crange(1)-!y.crange(0))*0.90
xyouts,0.05,yl,'P(d) ='+string(per,form='(f12.7)')+' =' + $
	string(per*24.0,form='(f10.4)')+' hr', charthick=1.5,charsize=2.0,font=1

yl=!y.crange(0)+(!y.crange(1)-!y.crange(0))*0.10
if keyword_set(finder) then begin
	lab = 'ErrLim: '+string(erl,form='(f6.2)')
	xyouts,0.34,yl,lab,charthick=1.5,charsize=2.0,font=1
endif else begin
	lab = 'JDRAN:   '+string(twin(0),form='(f9.1)')+ ' - ' + $
	string(twin(1),form='(f9.1)')+ $
	'        ErrLim: '+string(erl,form='(f6.2)')
	xyouts,0.05,yl,lab,charthick=1.5,charsize=2.0,font=1
endelse

endelse

;
; do the fitting

if keyword_set(fit) then begin
;
; fit phase

w	= 1.0/(icers)^2				; instrumental weights
;w	= fltarr(n_elements(phase)) + 1.0	; even weights

; sort by phase

t	= sort(phase)
phase	= phase(t)
icnts	= icnts(t)
w	= w(t)

; fit two cycles

w	= [w,w]
phase	= [phase,phase+1.0]
icnts	= [icnts,icnts]

;
; initial coefs
a	= [ double(vmean), double(vsig), 4.d0*!dpi, 0.d0, $
			   double(vsig/5.), 2.d0*!dpi, 0.d0 ]
;
; fit with single cos function (see perfunc.pro)
yfit = curvefit(phase,icnts,w,a,asig,chisq=chisq, function_name='perfunc', $
		/double, iter=nits)
avdev = avg(abs(yfit-icnts))

print,'Function params: '
print,a, form='(7f10.5)'
print,'Function sigmas: '
print,asig, form='(7f10.5)'
print,'Nits, Chisq    : ',strn(nits), ' ',strn(chisq)
print,'Avg deviation  : ',strn(avdev)

oplot,phase,yfit,thick=3

;
; now insert offsets for removing periodic signal

phase = lcur_com_get('phase')
coffs = a(1) * cos( a(2) * phase + a(3) ) + a(4) * sin( a(5) * phase + a(6) )
coffs = -coffs
ret = lcur_com_put('coffs',coffs)

;
; get alias signal

jd	= lcur_com_get('jd')
merrs	= lcur_com_get('merrs')
perfunc,phase,a,yfit
cnts2mag,yfit,amags

;
; print out minima
testp = [0.25, 0.5, 0.75, 1.0]
teste = [avdev, avdev, avdev, avdev]
perfunc,testp,a,testy
cnts2mag,testy,testm,teste,testme
if testm(0) lt testm(2) then begin
	maxm = testm(0)
	maxme= testme(0)
endif else begin
	maxm = testm(2)
	maxme= testme(2)
endelse

minav= avg([testm(1), testm(3)])
minave=avg([testme(1), testme(3)])

print,'ellipsoidal amp: ',minav-maxm,' +- ', avg([minave,maxme]), $
	form='(a,f7.3,a,f7.3)'
print,'min0.5 - min1.0: ', testm(1) - testm(3), ' +- ', minave, $
	form='(a,f7.3,a,f7.3)'

;
; write out alias file

pfile	= lcur_com_get('pfile')
root	= gettok(pfile,'.')
openw,olun,root+'.ali',/get_lun
printf,olun,n_elements(jd),filt,form='(i12,2x,a)'
for i=0,n_elements(jd)-1 do $
	printf,olun,jd(i),amags(i),merrs(i),form='(f12.4,f9.3,f9.3)'
free_lun,olun

;
; write out fit file

openw,olun,root+'.fit',/get_lun
printf,olun,a
printf,olun,chisq
free_lun,olun

endif

return
end
