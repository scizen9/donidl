pro lcur_set_pran
;
;
ipr0 = 0.
ipr1 = 0.
;
print,'CURRENT:'
print,'Use default pran: ',lcur_com_get('def_pran')
print,'Per range (days): ',strn( lcur_com_get('pran') )

q = ''

read, 'Enter pr0,pr1 or <cr> - default: ',q

if q ne '' then begin

	ipr0 = float(gettok(q,' '))
	ipr1 = float(q)

	if ( ipr0 lt ipr1 and ipr0 gt 0. ) then begin

		ret = lcur_com_put('def_pran', (1 eq 0) )
		ret = lcur_com_put('pran', [ipr0,ipr1] )

	endif else begin

		print,'LCUR_SET_PRAN: ERROR, nonsensical pran: ',ipr0, ipr1, $
			', using default'
		ret = lcur_com_put('def_pran', (1 eq 1) )

	endelse

endif	else 	ret = lcur_com_put('def_pran', (1 eq 1) )

lcur_analyze

return
end
