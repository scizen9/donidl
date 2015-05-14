function isorad,pa,x,y,inc=inc,q=q,ell=ell,b=b,silent=silent
;
; isorad - compute isophotal radius given axal ratio, position angle, offset
;	   NB: isophotal radius is equivalent to semi-major axis of ellipse
;	   upon which x,y are found
;
; INPUTS:
;	pa	- position angle of major axis in degrees
;	x,y	- offset from galaxy center in arcsec
;
; KEYWORDS:
;	inc	- inclination to line of sight in degrees
;	q	- axal ratio min diam / maj diam
;	ell	- ellipticity (1. - q)
;	b	- semi-minor axis in arcsec
; NB: must define one of these
;
; RETURN VALUE:
;	r	- isophotal radius in arcsec
;
; OPTIONAL OUTPUTS:
;	inc	- if q or ell provided, this will be returned in keyword var
;
; check keywords
if not (keyword_set(inc) or keyword_set(q) or keyword_set(ell) ) then begin
	if not keyword_set(silent) then $
		print,'ISORAD - Error: must define either inc, q, or ell'
	return,-1.
endif
;
; first get galaxy inclination in radians
if keyword_set(inc) then begin
	inr = inc / !radeg
endif else begin
	if keyword_set(ell) then $
		q = 1. - ell $
	else	ell = 1. - q
	inr = acos( sqrt ( (q^2-0.2^2)/(1.0-0.2^2) ) ) + 3.0/!radeg
endelse
;
; now convert offsets to radians
xr = x/(3600.*!radeg)
if x lt -9000. then xr = 0.
yr = y/(3600.*!radeg)
if y lt -9000. then yr = 0.
;
; angle alpha
if xr gt 0. then $
	alpha = atan(yr/xr) $
else	alpha = !pi/2.
;
; position angle - 90. deg to radians
pr = (pa - 90.)/!radeg
;
; get radius in radians
r2 = (xr^2 + yr^2) * ( cos(alpha + pr)^2 + $
	sin(alpha + pr)^2 * (1./cos(inr))^2 )
;
; convert to arcsec
r = sqrt(r2) * !radeg * 3600.d0
;
; semi-minor axis
b = r*cos(inr)
;
; convert inclination to degrees
inc = inr * !radeg
;
; check for edge on condition
if inc ge 90. then begin
	r = sqrt(x^2 + y^2)
	if not keyword_set(silent) then $
		print,'Warning: edge on galaxy, offset undefined'
endif
;
return,r
end
