       	function sky_fors, wave
;       flux from VLT Paranal in erg cm-2 s-2 A-1 arcsec-2
	fs = mrdfits('/Users/christophermartin/WORK/PROJECTS/KCWI/ETC/SKY/skyspectra/fxSKY.2002-04-07T07:38:53.802.fits',0,hdr)
	dw = sxpar(hdr, 'CDELT1')
	w0 = sxpar(hdr, 'CRVAL1')
	ns = n_elements(fs)
	ws = findgen(ns)*dw+w0
        wave1 = wave
        if keyword_set(ang) then wave1 = wave
        fs_int = interpol(fs,ws,wave1)
	ps_int = fs_int/(2.e-8/wave1)
        return, ps_int
        end

~
~
