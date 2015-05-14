pro gx_avmags,band,mgs,mge,expt,avmg,avme,texpt,nobs,avbckg,errlim=errlim, $
	background=background,error_weights=error_weights,silent=silent
;
; convert GALEX mags to fluxes and average
;
; if background keyword present, give precedence to obs with non-zero 
;	values (from NGA)
;
; setups
avmg = -99.
avme = -9.
texpt = 0.
nobs = 0
avbckg = 0.
;
; check band
if band eq 1 then zp = 20.08 else if band eq 2 then zp = 18.82 else begin
	if not keyword_set(silent) then $
		print,'Nonsensical band (NUV = 1, FUV = 2): ',band
	return
endelse
;
; good points
if keyword_set(errlim) then $
	erl = errlim $
else	erl = 1.5
g = where(mgs ge 0. and mgs lt 50. and mge lt erl, ng)
if ng lt 1 then begin
	if not keyword_set(silent) then $
		print,'No good data.'
	return
endif
gmgs = mgs(g)
gmge = mge(g) > 0.005
gexp = expt(g)
;
; do we have background?
if keyword_set(background) then begin	; give those with background precedence
	gbckg = background(g)
	n=where(gbckg gt 0., nobs)
	if nobs le 0 then begin
		nobs=ng
		gbckg = fltarr(nobs)
	endif else begin
		gmgs = gmgs(n)
		gmge = gmge(n)
		gexp = gexp(n)
		gbckg = gbckg(n)
	endelse
endif else begin
	nobs=ng
	gbckg = fltarr(nobs)
endelse
;
; perform average
if nobs gt 1 then begin
	nflx = 10.d0^(-0.4*(gmgs - zp))
	nflxe = (nflx * gmge)/1.0857362d0
	if keyword_set(error_weights) then begin
		wnflx = wmean(nflx,nflxe)
		wnflxe = wstdev(nflx,nflxe)
		avbckg = wmean(gbckg,nflxe)
	endif else begin
		wnflx = avg(nflx)
		wnflxe = stddev(nflx)
		avbckg = avg(gbckg)
	endelse
	if wnflx gt 0. then begin
		avmg = -2.5*alog10(wnflx) + zp
		avme = 1.0857362d0 * (wnflxe/wnflx)
		texpt = total(gexp)
	endif else begin
		avmg = -99.
		avme = -9.
		texpt = 0.
		nobs = 0
		avbckg = 0.0
	endelse
endif else if nobs eq 1 then begin
	avmg = gmgs(0)
	avme = gmge(0)
	texpt = gexp(0)
	avbckg = gbckg(0)
endif else begin
	avmg = -99.
	avme = -9.
	texpt = 0.
	avbckg = 0.
endelse
;
return
end
