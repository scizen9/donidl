pro lcur_phase_get
;

;print,'lcur_phase_get'
;
; get the data

jd	= lcur_com_get('jd')
cnts	= lcur_com_get('counts')
cers	= lcur_com_get('cerrs')
npts	= lcur_com_get('npts')
per	= lcur_com_get('period')

do_sub	= lcur_com_get('do_sub')

if do_sub then begin
	coffs	= lcur_com_get('coffs')
	cnts	= cnts + coffs
endif

;
; get phase

phase = (jd mod per) / per

;
; get phase 0

tphase = phase
phasebin,tphase,cnts,cers,50,/weight
good = where(cnts gt 0., ngood)
if ngood gt 0 then begin

	cnts = cnts(good)
	tphase = tphase(good)

	t = where(cnts eq min(cnts))
	ph0 = tphase(t(0))

	phase = phase - ph0

	neg = where(phase lt 0, nneg)

	if nneg gt 0 then $
		phase(neg) = 1.0 + phase(neg)
endif

;
; put the data
	
ret = lcur_com_put('phase', phase)

;
return
end	; pro lcur_phase_get
