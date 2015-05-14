pro lcur_data_get,ifile
;
if n_params(0) lt 1 then begin
	ifile = ''
	read,'phot file: ',ifile
endif

if strtrim(ifile,2) eq '' then return

rdphot,ifile, ifilt, ijd, imgs, imes

;
; check number of points
if ( lcur_com_put('jd',ijd) ) then begin

;
; get initial jdran, jdzer, and twin
	ret = lcur_com_put('jdran', [min(ijd),max(ijd)])
	ret = lcur_com_put('jdzero', (fix(min(ijd)/1000) * 1000.d0) )
	ret = lcur_com_put('twin', [min(ijd),max(ijd)])
	ret = lcur_com_put('t0', 0)
	ret = lcur_com_put('t1', (n_elements(ijd) - 1) )

;
; set initial pran to default
	ret = lcur_com_put('def_pran', (1 eq 1) )
;
; store in common variables

	ret = lcur_com_put('pfile', ifile)
	ret = lcur_com_put('filt', ifilt)
	ret = lcur_com_put('ntpts', n_elements(ijd))

	ret = lcur_com_put('mags', imgs)
	ret = lcur_com_put('merrs', imes)
	
	print,'lcur_data_get: ',strn(n_elements(ijd))+' points'
;
; analyze data
	lcur_analyze
	lcur_prt_pars

endif	
;
return
end	; pro lcur_data_get
