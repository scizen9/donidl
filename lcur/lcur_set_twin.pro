pro lcur_set_twin, ijd0, ijd1, noplot=noplot
;
;
; get jd vector
jd = lcur_com_get('jd')
ntpts = n_elements(jd)

if n_params(0) lt 1 then begin
	ijd0 = 0.d0
	ijd1 = 0.d0

	resp = ''

	print,'CURRENT:'
	print,'JD range (days) : ',strn( lcur_com_get('jdran') )
	print,'Time window     : ',strn( lcur_com_get('twin') )
	read, 'New window 0,1  : ',resp

	if strmid(resp,0,1) eq 'd' then begin
		day = fix(strmid(resp,1))
		ljd = long(jd) 
		ljd = ljd(sort(ljd)) 
		ljd = ljd(uniq(ljd))
		nljd = n_elements(ljd)
		if day gt 0 and day le nljd then begin
			ijd0 = double(ljd(day-1))
			ijd1 = ijd0 + 1.d0
		endif else begin
			ijd0 = double(ljd(0))
			ijd1 = double(ljd(nljd-1)) + 1.d0
		endelse
	endif else begin
		ijd0 = double(gettok(resp,','))
		ijd1 = double(gettok(resp,','))
	endelse

endif

if ( ijd0 lt ijd1 ) and ( ijd0 lt max(jd) ) and ( ijd1 gt min(jd) ) then begin

	ijd0 = ijd0 > min(jd)
	ijd1 = ijd1 < max(jd)

	ret = lcur_com_put('twin', [ijd0, ijd1] )

	t = where(jd ge ijd0)
	ret = lcur_com_put('t0', (t(0) > 0) )

	t = where(jd gt ijd1, count)

	if ( count gt 0 ) then $
		ret = lcur_com_put('t1', ( ( t(0) - 1 ) < (ntpts-1) > 0 ) ) $
	else	ret = lcur_com_put('t1', ntpts-1)

	if not keyword_set(noplot) then begin
		lcur_analyze
		lcur_plt_phot
	endif

endif else $
	print,'ERROR: nonsensical window: ',ijd0, ijd1,', no changes made.'
;
return
end
