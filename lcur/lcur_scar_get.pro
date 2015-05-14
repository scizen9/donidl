pro lcur_scar_get
;
;print,'lcur_scar_get'

;
; get the data

ijd	= lcur_com_get('jd')
imgs	= lcur_com_get('mags')
imes	= lcur_com_get('merrs')

;
; convert to counts

mag2cnts, ijd, imgs, imes, -1, itim, icnts, icers

;
; put the data
	
if ( lcur_com_put('time',itim) ) then begin
	ret = lcur_com_put('counts', icnts)
	ret = lcur_com_put('cerrs', icers)
endif

;
return
end	; pro lcur_scar_get
