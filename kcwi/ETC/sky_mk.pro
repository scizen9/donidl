	function sky_mk, wave, grat, ang=ang
;	flux from (MK?) sky in  phot/s/nm/arcsec^2/m^2
	readcol, 'mk_sky.dat', ws, fs
	f_nu = mrdfits('lris_esi_skyspec_fnu_uJy.fits',0,hdr)
     	dw = sxpar(hdr, 'CDELT1')
        w0 = sxpar(hdr, 'CRVAL1')
        ns = n_elements(fs)
        ws = findgen(ns)*dw+w0
	f_lam = f_nu*1.d-29*3.d18/ws/ws
	p_lam = f_lam/(2.d-8/ws)
	ps_int = interpol(p_lam,ws,wave)
	return, ps_int
	end

