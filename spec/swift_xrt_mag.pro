pro swift_xrt_mag,flam,swav,xrtmab,xrtefflam,xrtfwhm,xrtfwidth,xrtmaberr,$
	flerr=flerr, silent=silent,wt=wt,pc=pc,arf=arf
;+
; swift_xrt_mag - compute SWIFT XRT magnitudes from a spectrum
;
; INPUTS:
;	flam	- spectrum in f_lambda units
;	swav	- wavelengths in angstroms
;
; OUTPUTS:
;	xrtmab	- XRT AB magnitude
;	xrtefflam - XRT effective wavelength (angstroms)
;	xrtfwhm	- XRT Gaussian FWHM (angstroms)
;	xrtfwidth - XRT RMS fractional width (unitless)
;	xrtmaberr - XRT error in AB magnitude (if flerr set)
;
; KEYWORDS:
;	flerr	- flux error in f_lambda units
;	silent	- to suppress output
;	wt	- set to use windowed timing mode
;	pc	- set to use photon counting mode (default)
;
;-
nw  = n_elements(flam)
if nw ne n_elements(swav) then begin
    print,'SWIFT_XRT_MAG: Error - flam and swav must have same number of elements'
    return
endif
if keyword_set(flerr) then begin
    if nw ne n_elements(flerr) then begin
    	print, $
	'SWIFT_XRT_MAG: Error - flam and flerr must have same number of elements'
    	return
    endif
endif

; read Effective Area table
if not keyword_set(arf) then begin
arfil = '/Users/neill/catvars/uvflash/swxpc0to12s6_20010101v011_arf.fits'
if keyword_set(wt) then $
	arfil = '/Users/neill/catvars/uvflash/swxwt0to2s6_20010101v012_arf.fits'
arf=mrdfits(arfil,1,ahdr)
endif
xea=arf.specresp
xwav=(12.41)/arf.energ_hi
;gx_read_ea,fea,fwav,nea,nwav

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
; XRT
if min(swav) lt 2. and max(swav) gt 100. then begin
	xrt_pass	= interpol(xea,xwav,swav)>0.
	xrt_fnu		= total(dlognu*spec_fnu*xrt_pass)/ $
				total(dlognu*xrt_pass)
	if keyword_set(flerr) then begin
		xrt_err	= total(dlognu*err_fnu*xrt_pass)/ $
				total(dlognu*xrt_pass)
		xrtmaberr = 1.0857362 * xrt_err / xrt_fnu
	endif else xrtmaberr = -99.9
	xrtmab		= -2.5 * alog10( xrt_fnu ) - 48.60
	xrtefflam	= exp(total(dlnnu*xrt_pass*alog(swav))/ $
					total(dlnnu*xrt_pass))
	xrtfwidth	= sqrt(total(dlnnu*xrt_pass*(alog((swav)/ $
					xrtefflam))^2)/total(dlnnu*xrt_pass))
	xrtfwhm		= 2.*sqrt(2.*alog(2.))*xrtfwidth*xrtefflam
endif else begin
	xrtmab		= -99.9
	xrtmaberr	= -99.9
	xrtefflam	= -99.9
	xrtfwidth	= -99.9
	xrtfwhm		= -99.9
endelse


if not keyword_set(silent) then begin
	fmt='(a,2f10.3)'
	print,'XRT  (ABmag)    : ', xrtmab,format=fmt
	if keyword_set(flerr) then $
	print,'XRTerr          : ', xrtmaberr,format=fmt
	print,'XRTefflam  (Ang): ', xrtefflam,format=fmt
	print,'XRTfwhm    (Ang): ', xrtfwhm,  format=fmt
	print,'XRTfwidth  (Ang): ', xrtfwidth,format=fmt
endif

return
end
