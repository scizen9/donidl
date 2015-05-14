pro lcur_set_varth,ilim
;
;

COMMON dao_data

if n_params(0) lt 1 then begin
	print,'CURRENT:'
	print,'Variab Thresh: ',strn( lcur_com_get('dao_varth') )

	ilim = [0.,0.]
	read, 'New Thresh   : ',ilim
endif

if ( ilim(0) ge 0. and ilim(1) ge 0. and ilim(1) ge ilim(0)) then begin
	
	t = where(dao_var ge ilim(0) and dao_var le ilim(1) and $
		  dao_rnei ge dao_neith and dao_enframes ge dao_nflim, nfound)

	if nfound le 0 then $
		print,'ERROR: no stars with '+strn(ilim(0))+' < var < ', strn(ilim(1))+', no changes made.' $
	else	begin
		print,nfound,' Stars with '+strn(ilim(0))+' < var < ', strn(ilim(1))+' found.'
		ret = lcur_com_put('dao_varth', ilim )
		ret = lcur_com_put('dao_nvar', nfound )
	endelse

endif else $
	print,'ERROR: nonsensical threshhold: ',ilim,', no changes made.'
;
return
end
