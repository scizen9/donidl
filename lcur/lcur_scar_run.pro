pro lcur_scar_run
;
print,'lcur_scar_run'
npts		= lcur_com_get('npts')
def_pran	= lcur_com_get('def_pran')

if npts gt 0 then begin

	itim	= lcur_com_twin('time')
	icnt	= lcur_com_twin('counts')
	ierr	= lcur_com_twin('merrs')
	errlim	= lcur_com_get('merrlim')
	do_sub	= lcur_com_get('do_sub')
	if do_sub then begin
		coffs = lcur_com_twin('coffs')
		icnt = icnt + coffs
	endif
	
	good = where(ierr le errlim, ngood)
	if ngood le 10 then begin
		print,'LCUR_SCAR_RUN: ERROR, error limit too strict; only ', $
			strn(ngood),' points left.'
		return
	endif

	itim = itim(good)
	icnt = icnt(good)

	if def_pran then begin
		scargle,itim,icnt,om,ipx,period=ipd,noise=inoise,nif=inif, $
			nu=inu,fmin=ifmin,fmax=ifmax,signi=isigni
	endif else begin
		pran = lcur_com_get('pran')
		scargle,itim,icnt,om,ipx,period=ipd,noise=inoise,nif=inif, $
			nu=inu,fmin=ifmin,fmax=ifmax,signi=isigni, $
			pmin=pran(0),pmax=pran(1)
	endelse
;
; insert into common variables
	if ( lcur_com_put('nu',inu) ) then begin

		ret = lcur_com_put('nif', inif)
		ret = lcur_com_put('signi', isigni)
		ret = lcur_com_put('noise', inoise)
		ret = lcur_com_put('fmin', ifmin)
		ret = lcur_com_put('fmax', ifmax)
		if def_pran then $
			ret = lcur_com_put('pran',[ 1.0 / ifmax, 1.0 / ifmin ])

		npks = lcur_com_get('npkmax')

		perpeaks,ipx,npks,ipkis
		ret = lcur_com_put('pkis', ipkis)
		ret = lcur_com_put('period', ipd(ipkis(0)) )
		lcur_phase_get

		errpeaks,inu,ipx,ipkis,npks,pkerrs
		ret = lcur_com_put('pkerrs', pkerrs)

		ifaps = alog10(1.d0 - (1.d0 - exp(-ipx))^inif)

		ret = lcur_com_put('pd', ipd)
		ret = lcur_com_put('px', ipx)
		ret = lcur_com_put('faps', ifaps)
	endif
endif else print,'No data read in yet'

return
end	; pro lcur_scar_run
