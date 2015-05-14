pro gx_read_poly,fply,nply,forder=forder,norder=norder,inverse=inverse
;+
; GX_READ_POLY	- read polynomials for GALEX wavelength solution
;		wavelength to arcsec
;
;	gx_read_poly, fuv_poly, nuv_poly [, fuv_order=n, nuv_order=n, /inverse ]
;
; INPUTS:
;
;	None
;
; OUTPUT:
;
;	fply	- FUV polynomial coefficients
;	nply	- NUV polynomial coefficients
;
; KEYWORDS:
;
;	forder	- the FUV spectral order to return (defaults to 2)
;	norder	- the NUV spectral order to return (defaults to 1)
;	inverse	- set this to return inverse polynomials (arcsec to wave)
;
; HISTORY:
;
;	12oct07 jdn	- intial revision
;-
;
if n_params(0) lt 1 then begin
	print, 'Usage: gx_read_poly, fuv_poly, nuv_poly [, forder=n, norder=n, /inverse]'
	return
endif
;
; set ups
rec=''
;
; where is the polynomial data?
plydir = '~/soda/calib/spec/poly/'
;
; get FUV input file name
if keyword_set(forder) then begin
	if forder lt 0 then $
		sg = '-' $
	else	sg='+'
	fo = sg+strn(abs(forder))
endif else fo = '+2'
if keyword_set(inverse) then $
	fuvpfl = plydir+'fuv_order'+fo+'.ipoly' $
else	fuvpfl = plydir+'fuv_order'+fo+'.poly'
;
; read FUV coefs
openr,il,fuvpfl,/get_lun
readf,il,rec
fpo = fix(gettok(rec,' '))
fply = dblarr(fpo+2)
for i=0,fpo+1 do begin
	readf,il,rec
	fply(i) = double(gettok(rec,' '))
endfor
free_lun,il
;
; get NUV input file name
if keyword_set(norder) then begin
	if norder lt 0 then $
		sg = '-' $
	else	sg='+'
	no = sg+strn(abs(norder))
endif else no = '+1'
if keyword_set(inverse) then $
	nuvpfl = plydir+'nuv_order'+no+'.ipoly' $
else	nuvpfl = plydir+'nuv_order'+no+'.poly'
;
; read NUV coefs
openr,il,nuvpfl,/get_lun
readf,il,rec
npo = fix(gettok(rec,' '))
nply = dblarr(npo+2)
for i=0,npo+1 do begin
	readf,il,rec
	nply(i) = double(gettok(rec,' '))
endfor
free_lun,il
;
return
end
