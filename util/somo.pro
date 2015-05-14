pro somo,ra,dec,dpc,dpmra,dpmdec,pmra,pmdec,vrad,silent=silent
;
; inputs:
;	ra,dec	- coords in decimal degrees
;	dpc	- distance in parsecs
;
; outputs:
;	dpmra,dpmdec	- corrections to pm in mas/yr
;
; solar motion from Dehnen and Binney (1998)
; also see Astrophysical Quantities 4ed S19.11
;
; check inputs
if n_params(0) lt 3 then begin
	print,'SOMO: Usage - somo, ra, dec, dpc, dpmra, dpmdec, pmra, pmdec, vrad <, /silent>'
	return
endif
;
; setups
vs = 13.4d0
ras = 270.d0
decs= 30.d0
;ras = 251.d0
;decs= 10.d0
rap = ras - 180.d0
decp= -decs
fmt='(a,3f9.3)'
;
; first project velocity to position in sky
gcirc,2,ras,decs,ra,dec,dis
dis = dis / 3600.d0 / !radeg	; convert asec to radians
vp = sin(dis) * vs		; project to objects position
;
if not keyword_set(silent) then begin
	print,'Projected Solar Motion: '
	print,'      velocity (km/s)         : ',vp,form=fmt
endif
;
; now get angular motion at input distance
; use small angle approximation a/b ~ sin(alpha) for large b
angsec = asin( vp / (dpc * 3.0856776d13) ) * !radeg * 3600.d0 * 1000.d0 * $
		365.25d0 * 24.d0 * 3600.d0	; convert to mas/yr
if not keyword_set(silent) then $
	print,'      sky motion (mas/yr)     : ',angsec,form=fmt
;
; now project onto ra and dec grid
a1 = ra / !radeg
b1 = dec / !radeg
;
if dis le !pi/2. then begin
	a2 = ras / !radeg
	b2 = decs / !radeg
endif else begin
	a2 = rap / !radeg
	b2 = decp / !radeg
endelse
y = sin(a2 - a1) * cos(b2)
x = sin(b2) * cos(b1) - cos(b2) * sin(b1) * cos(a2 - a1)
if x ne 0. or y ne 0. then $
	alpha = atan(y, x) $
else	alpha = 0.
;
;
if not keyword_set(silent) then $
	print,'      position angle (deg)    : ',alpha*!radeg,format=fmt
;
if dis le !pi/2. then begin
	dpmra = angsec * sin(alpha)
	dpmdec = angsec * cos(alpha)
endif else begin
	dpmra = -angsec * sin(alpha)
	dpmdec = -angsec * cos(alpha)
endelse
if not keyword_set(silent) then begin
	print,'Object Motion:'
	print,'      Del pm: ra, dec (mas/yr): ',dpmra,dpmdec,format=fmt
endif
;
if n_params(0) gt 5 then begin
	gal_uvw,u,v,w,dis=dpc,ra=ra,dec=dec,pmra=pmra,pmdec=pmdec,vrad=vrad,/lsr
	spvel=sqrt(u^2+v^2+w^2)
	rm=dpc*3.086d13	; distance in km
	vra=(pmra+dpmra) / 1000.0	; arcsec
	vra=vra * rm / 206265.d0	; km/yr
	vra=vra / (3600.d0 * 24.d0 * 365.25d0)	; km/s
	vdc=(pmdec + dpmdec) / 1000.0
	vdc=vdc * rm / 206265.d0
	vdc=vdc / (3600.d0 * 24.d0 * 365.25d0)
	vr = sqrt(spvel^2-vra^2-vdc^2)
	if vra ne 0. or vdc ne 0. then $
		alpha = atan(vra,vdc)  $
	else	alpha = 0.
	vsky = sqrt(vra^2+vdc^2)
	if vsky ne 0. or vr ne 0. then $
		inc   = atan(vsky,vr)  $
	else	inc   = 0.
	print,'      uvw space vel, (km/s)   : ',spvel,format=fmt
	print,'      vra, vdec, |vr| (km/s)  : ',vra,vdc,vr,format=fmt
	print,'      position angle (deg)    : ',alpha*!radeg,format=fmt
	print,'      LOS inclination (deg)   : ',inc*!radeg,format=fmt
endif
;
return
end
