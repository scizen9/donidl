pro specflux,ifl,density=density,nonegative=nonegative
;
; write out ASCII file of fluxes
;
; density - set to the conversion factor to convert fluxes to flux density
; nonegative - set to prevent negative fluxes (set to 0.)
;
; check keywords and parameters
den=1.
if keyword_set(density) then $
	if density gt 0. then den=density
;
if n_params(0) lt 1 then begin
	ifl=''
	read,'filename: ',ifl
endif
;
; get file
if strpos(ifl,'.') lt 0 then begin
	rfl = ifl+'.fits'
	ofl = ifl+'.dat'
endif else begin
	rfl = ifl
	tmp = ifl
	rute = gettok(tmp,'.')
	ofl = rute+'.dat'
endelse
;
; read spectrum
spec=mrdfits(rfl,0,h,/silent)
;
; process
if keyword_set(nonegative) then $
	flx = (spec * den) > 0. $
else	flx = spec * den
;
; get wavelength solution from header
w0=sxpar(h,'CRVAL1')
dw=sxpar(h,'CDELT1')
nw=sxpar(h,'NAXIS1')
wave=findgen(nw)*dw + w0
;
; data units
bu=sxpar(h,'BUNIT')
;
; output file
openw,ol,ofl,/get_lun
;
; print header
printf,ol,'# SPECFLUX: '+systime(0)
printf,ol,'# file: ',rfl
printf,ol,'# Flux unit: ',bu
if keyword_set(density) then $
	printf,ol,'# Density conversion applied: ',den
printf,ol,'# Wave(A)    Flux'
;
for i=0,nw-1 do printf,ol,wave(i),flx(i),format='(f9.2,g13.4)'
;
free_lun,ol
;
return
end

