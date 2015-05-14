pro gx_spec_mag,flam,swav,fuvmab,nuvmab,fuvefflam,nuvefflam, $
	fuvfwhm,nuvfwhm,fuvfwidth,nuvfwidth,fuvmaberr,nuvmaberr, $
	flerr=flerr, silent=silent
;+
; gx_spec_mag - compute GALEX AB magnitudes from a spectrum
;
; INPUTS:
;	flam	- spectrum in f_lambda units
;	swav	- wavelengths in angstroms
;
; OUTPUTS:
;	fuvmab	- FUV AB magnitude
;	nuvmab	- NUV AB magnitude
;	fuvefflam - FUV effective wavelength (angstroms)
;	nuvefflam - NUV effective wavelength (angstroms)
;	fuvfwhm	- FUV Gaussian FWHM (angstroms)
;	nuvfwhm	- NUV Gaussian FWHM (angstroms)
;	fuvfwidth - FUV RMS fractional width (unitless)
;	nuvfwidth - NUV RMS fractional width (unitless)
;	fuvmaberr - FUV error in AB magnitude (if flerr set)
;	nuvmaberr - NUV error in AB magnitude (if flerr set)
;
; KEYWORDS:
;	flerr	- flux error in f_lambda units
;	silent	- to suppress output
;
;-
nw  = n_elements(flam)
if nw ne n_elements(swav) then begin
    print,'GX_SPEC_MAG: Error - flam and swav must have same number of elements'
    return
endif
if keyword_set(flerr) then begin
    if nw ne n_elements(flerr) then begin
    	print, $
	'GX_SPEC_MAG: Error - flam and flerr must have same number of elements'
    	return
    endif
endif

; read Effective Area table
gx_read_ea,fea,fwav,nea,nwav

;Convert f_lam to f_nu,
dlam      = swav - shift(swav,1)
dlam(0)   = dlam(1)
spec_nu   = 2.99792458e18/swav	;Hz
dnu       = shift(spec_nu,1) - spec_nu
dnu(0)    = dnu(1)
dlognu    = shift(alog10(spec_nu),1) - alog10(spec_nu)
dlognu(0) = dlognu(1)
dlognu    = median(dlognu,3)
dlnnu     = shift(alog(spec_nu),1) - alog(spec_nu)
dlnnu(0)  = dlnnu(1)
dlnnu     = median(dlnnu,3)

;Compute reference magnitudes   
spec_fnu  = flam*(dlam/dnu)
spec_mab  = -2.5*alog10(spec_fnu)-48.60 ;Oke 1974
if keyword_set(flerr) then $
	err_fnu = flerr*(dlam/dnu)


;Compute passbands
; FUV
if min(swav) lt 1400. and max(swav) gt 1800. then begin
	fuv_pass	= interpol(fea,fwav,swav)
	fuv_fnu		= total(dlognu*spec_fnu*fuv_pass)/ $
				total(dlognu*fuv_pass)
	if keyword_set(flerr) then begin
		fuv_err	= total(dlognu*err_fnu*fuv_pass)/ $
				total(dlognu*fuv_pass)
		fuvmaberr = 1.0857362 * fuv_err / fuv_fnu
	endif else fuvmaberr = -99.9
	fuvmab		= -2.5 * alog10( fuv_fnu ) - 48.60
	fuvefflam	= exp(total(dlnnu*fuv_pass*alog(swav))/ $
					total(dlnnu*fuv_pass))
	fuvfwidth	= sqrt(total(dlnnu*fuv_pass*(alog((swav)/ $
					fuvefflam))^2)/total(dlnnu*fuv_pass))
	fuvfwhm		= 2.*sqrt(2.*alog(2.))*fuvfwidth*fuvefflam
endif else begin
	fuvmab		= -99.9
	fuvmaberr	= -99.9
	fuvefflam	= -99.9
	fuvfwidth	= -99.9
	fuvfwhm		= -99.9
endelse

; NUV
if min(swav) lt 1900. and max(swav) gt 2800. then begin
	nuv_pass	= interpol(nea,nwav,swav)
	nuv_fnu		= total(dlognu*spec_fnu*nuv_pass)/ $
					total(dlognu*nuv_pass)
	if keyword_set(flerr) then begin
		nuv_err	= total(dlognu*err_fnu*nuv_pass)/ $
				total(dlognu*nuv_pass)
		nuvmaberr = 1.0857362 * nuv_err / nuv_fnu
	endif else nuvmaberr = -99.9
	nuvmab		= -2.5 * alog10( nuv_fnu ) - 48.60
	nuvefflam	= exp(total(dlnnu*nuv_pass*alog(swav))/ $
					total(dlnnu*nuv_pass))
	nuvfwidth 	= sqrt(total(dlnnu*nuv_pass*(alog((swav)/ $
					nuvefflam))^2)/total(dlnnu*nuv_pass))
	nuvfwhm		= 2.*sqrt(2.*alog(2.))*nuvfwidth*nuvefflam
endif else begin
	nuvmab		= -99.9
	nuvmaberr	= -99.9
	nuvefflam	= -99.9
	nuvfwidth	= -99.9
	nuvfwhm		= -99.9
endelse

if not keyword_set(silent) then begin
	fmt='(a,2f10.3)'
	print,'FUV,       NUV (ABmag)    : ', fuvmab,nuvmab,format=fmt
	if keyword_set(flerr) then $
	print,'FUVerr,    NUVerr         : ', fuvmaberr,nuvmaberr,format=fmt
	print,'FUVefflam, NUVefflam (Ang): ', fuvefflam,nuvefflam,format=fmt
	print,'FUVfwhm,   NUVfwhm   (Ang): ', fuvfwhm,  nuvfwhm,format=fmt
	print,'FUVfwidth, NUVfwidth,(Ang): ', fuvfwidth,nuvfwidth,format=fmt
endif

return
end
