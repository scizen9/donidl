pro gx_dc_cnts,file,id,wave,cnts,errs,ra=ra,dec=dec
;+
; GX_DC_CNTS - dispersion correct extracted counts
;
;-
gx_read_xsp,file,id,offs,rcnts,rerrs,ra=ra,dec=dec
;
; get inverse polynomial coefficients
gx_read_poly,fply,nply,/inverse
;
; get band
if strpos(file,'-fg-') ge 0 then begin
	band = 2 
	cofs = fply
	minw = 1280.
endif else begin
	band = 1
	cofs = nply
	minw = 1400.
endelse
;
; calculate wavelengths
zwave = poly( offs - cofs(0), cofs(1:*))
;
; get good range
t=where(zwave eq max(zwave))
zwave = zwave(0:t(0))
rcnts = rcnts(0:t(0))
rerrs = rerrs(0:t(0))
;
; get angstroms per arcsec
ranga = fltarr(n_elements(zwave)) + 1.0
for i=1,n_elements(zwave)-1 do ranga(i) = zwave(i) - zwave(i-1)
;
; now correct dispersion
w0 = 1200.0
w1 = 3100.0
nw = fix(w1-w0)+1
wave = findgen(nw)+w0
anga = interpol(ranga,zwave,wave)
cnts = interpol(rcnts,zwave,wave) / anga
errs = interpol(rerrs,zwave,wave)
m = where(wave lt minw, nm)
if nm gt 0 then begin
	cnts(m) = 0.
	errs(m) = 0.
endif
;
return
end
