pro lcur_set_rnei,ilim
;
;

if n_params(0) lt 1 then begin
	print,'CURRENT:'
	print,'Neighbor lim (r px): ',strn( lcur_com_get('dao_neith') )

	ilim = 0.
	read, 'New Neighbor lim: ',ilim
endif

if ( ilim ge 0. ) then begin

	ret = lcur_com_put('dao_neith', ilim )

endif else $
	print,'ERROR: nonsensical neighbor limit: ',ilim,', no changes made.'
;
return
end
