pro lcur_set_merr,ilim
;
;

if n_params(0) lt 1 then begin
	print,'CURRENT:'
	print,'Mag Error lim: ',strn( lcur_com_get('merrlim') )

	ilim = 0.
	read, 'New Error lim: ',ilim
endif

if ( ilim gt 0. ) then begin

	ret = lcur_com_put('merrlim', ilim )
	nstars = lcur_com_get('dao_nstars')
	if nstars gt 0 then begin
	    merrs = lcur_com_get('dao_merrs')
	    enframes=intarr(nstars)
	    for i=0,nstars-1 do begin
		t=where(merrs(*,i) le ilim, n)
		enframes(i) = fix(n)
	    endfor
	    ret = lcur_com_put('dao_enframes', enframes)
	endif

endif else $
	print,'ERROR: nonsensical error limit: ',ilim,', no changes made.'
;
lcur_analyze
;
return
end
