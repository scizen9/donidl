pro gx_read_ea,fea,fwav,nea,nwav,forder=forder,norder=norder,test=test
;+
; GX_READ_EA	- read effective area files
;
;	gx_read_ea, fuv_ea, fuv_wave, nuv_ea, nuv_wave [, fuv_order=n, nuv_order=n ]
;
; INPUTS:
;
;	None
;
; OUTPUT:
;
;	fea	- FUV effective area in cm^2
;	fwav	- FUV wavelengths for effective area curve (Angstroms)
;	nea	- NUV effective area in cm^2
;	nwav	- NUV wavelengths for effective area curve (Angstroms)
;
; KEYWORDS:
;
;	forder	- the FUV spectral order to return (defaults to 2)
;	norder	- the NUV spectral order to return (defaults to 1)
;	test	- filename to use in test calibrations, generated with
;			gx_ea_cal.pro
;
; HISTORY:
;
;	03oct07 jdn	- intial revision
;-
;
if n_params(0) lt 1 then begin
	print, 'Usage: gx_read_ea, fuv_ea, fuv_wave, nuv_ea, nuv_wave [, forder=n, norder=n, test=test_file]'
	return
endif
;
; check test keyword
if keyword_set(test) then begin
	readcol,test,fwav,fea,nea,skip=2,format='f,f,f'
	nwav=fwav
endif else begin
;
; where is the effective area data?
;	eadir = '~/soda/calib/ea_data/'
	eadir = '~/ref/calib/galex/ea_data/'
;
; read FUV ea
	ffl = eadir+'fuv-moea.fits'
	fuvea = mrdfits(ffl,1,fhdr,/silent)
;
; get ea and wave
	if keyword_set(forder) then $
		fo = forder + 5 $
	else	fo = 7
	fea = fuvea.(fo)
	fwav= fuvea.wavelength
;
; read NUV ea
	nfl = eadir+'nuv-moea.fits'
	nuvea = mrdfits(nfl,1,nhdr,/silent)
;
; get ea and wave
	if keyword_set(norder) then $
		no = norder + 5 $
	else	no = 6
	nea = nuvea.(no)
	nwav= nuvea.wavelength
;
endelse
;
return
end
