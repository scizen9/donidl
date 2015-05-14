pro lcur_set_bin,ibins,do_bin=do_bin,do_weight=do_weight
;
;

if n_params(0) lt 1 then begin
	print,'CURRENT:'
	print,'Period (days): ',strn( lcur_com_get('period') )
	print,'Do Binning   : ',strn( lcur_com_get('do_bin') )
	print,'Number o Bins: ',strn( lcur_com_get('bins') )
	print,'Do Wgted Avg : ',strn( lcur_com_get('do_weight') )
	q = ''

	read, 'Do Binning (y/n): ',q
	if q ne 'n' then begin
		ret = lcur_com_put('do_bin', (1 eq 1) )
		ibins = 0
		read,'Number of Bins: ',ibins
		ret = lcur_com_put('bins', ibins )
		r = ''
		read,'Do Weighted Avg? (y/n): ',r
		if r ne 'n' then $
			ret = lcur_com_put('do_weight', (1 eq 1) ) $
		else	ret = lcur_com_put('do_weight', (1 eq 0) )
	endif	else	ret = lcur_com_put('do_bin', (1 eq 0) )
endif else begin
	if keyword_set(do_bin) then $
		ret = lcur_com_put('do_bin', (1 eq 1) ) $
	else	ret = lcur_com_put('do_bin', (1 eq 0) )
	if keyword_set(do_weight) then $
		ret = lcur_com_put('do_weight', (1 eq 1) ) $
	else	ret = lcur_com_put('do_weight', (1 eq 0) )
	ret = lcur_com_put('bins',ibins)
endelse

lcur_phase_get

;
return
end
