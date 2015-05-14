pro gx_mcat_mags,mfil,ra0,dec0,rad,fmag,fmge,nmag,nmge,time
	mc = gx_mcat(mfil,ra0,dec0,rad,status=stat,time=time)
	if stat eq 0 then begin
		fmag = max(mc.fuv_mag_aper_5) + 18.82
		fmge = max(mc.fuv_magerr_aper_5)
		nmag = max(mc.nuv_mag_aper_5) + 20.08
		nmge = max(mc.nuv_magerr_aper_5)
	endif else begin
		fmag = -99.999
		fmge = -99.999
		nmag = -99.999
		nmge = -99.999
	endelse
return
end
