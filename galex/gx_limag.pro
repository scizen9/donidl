function gx_limag,expt,bckg,apr,fuv=fuv,nuv=nuv,verbose=verbose
;
; calculate limiting magnitudes given exposure time, background, and ap radius
;
; INPUTS:
;
;	expt	- exposure time in seconds
;	bckg	- background count rate per pixel
;	apr	- aperture radius in pixels
;
; init
;
lmg = -99.0
if not (keyword_set(fuv) or keyword_set(nuv)) then begin
	print,'GX_LIMAG: Error - choose fuv or nuv with keywords; /fuv of /nuv'
	return,lmg
endif
;
; number of pixels
npx = !pi * apr^2
;
; background counts
bc = npx * bckg * expt
;
; background noise
if keyword_set(fuv) then begin
	sn0 = 6.0
	dc = npx * 1.0d-3 * expt
endif else begin
	sn0 = 6.2
	dc = npx * 1.7d-2 * expt
endelse
;
; perturb until we get S/N = 5. +- 0.001
eps = 0.01
c = 25.0
sn = 3.0
i=0L
while i lt 10 or abs(sn - sn0) gt eps do begin
	i = i + 1L
	del = sn0 - sn
	if del ge 0. then $
		c = c + eps $
	else	c = c - eps
	sn = c / sqrt(c + bc + dc)
endwhile
;
; print bsig
if keyword_set(verbose) then $
	print,'Its,Counts,Bckg,s/n: ',i,c,bc,sn,format='(a,i5,4f9.3)'
;
; use a 5 sigma source limit sqrt(25) = 5 -> S/N = 5. assuming Poisson stats
if keyword_set(fuv) then $
	lmg = -2.5 * alog10( c / expt ) + 18.82 $
else	lmg = -2.5 * alog10( c / expt ) + 20.08
;
return,lmg
end
