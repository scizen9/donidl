pro lcur_set_fran
;
;
ifr0 = 0.
ifr1 = 0.
;
print,'CURRENT:'
print,'Use default fran: ',lcur_com_get('def_fran')
print,'Freq range (1/d): ',strn( lcur_com_get('fran') )

q = ''

read, 'Enter fr0,fr1 or <cr> - default: ',q

if q ne '' then begin

	ifr0 = float(gettok(q,' '))
	ifr1 = float(q)

	nu = lcur_com_get('nu')

	if ( ifr0 lt ifr1 and ifr0 lt max(nu) and ifr1 gt min(nu) ) then begin

		ret = lcur_com_put('def_fran', (1 eq 0) )

		ifr0 = ifr0 > min(nu)
		ifr1 = ifr1 < max(nu)

		ret = lcur_com_put('fran', [ifr0,ifr1] )

	endif else begin

		print,'LCUR_SET_PRAN: ERROR, nonsensical fran: ',ifr0, ifr1, $
			', using default'
		ret = lcur_com_put('def_fran', (1 eq 1) )

	endelse

endif	else 	ret = lcur_com_put('def_fran', (1 eq 1) )

return
end
