pro lcur_set_per,iper
;
;
COMMON scar_data

if n_params(0) lt 1 then begin
	print,'CURRENT:'
	print,'Period (days), Bin?, Nbin, Wgt?: ', lcur_com_get('period'), $
		lcur_com_get('do_bin'), lcur_com_get('bins'), $
		lcur_com_get('do_weight')

	iper = 0.d0
	cper = ''
	read, 'New Period: ',cper
	if strpos(cper,'.') ge 0 then $
		iper=double(cper) $
	else begin
		icper=fix(cper)-1
		if icper ge 0 and icper lt 10 then begin
			iper=pd(pkis(icper)) 
			print, 'New Period: ', strn(iper)
		endif else	iper=0.0
	endelse
endif

if ( iper gt 0. ) then begin

	ret = lcur_com_put('period', iper )

	lcur_phase_get


endif else $
	print,'ERROR: nonsensical period: ',iper,', no changes made.'
;
return
end
