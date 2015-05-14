pro get_fwhm2,im,fwhm,nstar,maxlim=maxlim,nsigma=nsigma,background=background,backsig=backsig
;+
; get_fwhm2 - get the fwhm of stars in IM
;-
; check keywords
if keyword_set(maxlim) then $
	maxl = maxlim $
else	maxl = max(im)
if keyword_set(nsigma) then $
	nsig = nsigma $
else	nsig = 5
;
; PARAMETERS:
roundlim = [-0.8,0.8]
sharplim = [0.2,1.0]
ifwhm = 5.0		; initial guess at fwhm
;
; get dimensions of image
s = size(im)
nx = s(1) & ny = s(2)
edge = 25
;
; get sky value
sky,im,background,skysig,/silent
backsig=skysig
hmin = background + nsig * skysig
nstar = myfind(im,x,y,flux,sharp,round,hmin,ifwhm,roundlim,sharplim,/silent)
fwhms = fltarr(nstar)
for i = 0,nstar-1 do begin
	if x(i) gt edge and x(i) lt (nx-edge) and $
	   y(i) gt edge and y(i) lt (ny-edge) then $
	begin
	    ssim = im( x(i)-16:x(i)+15, y(i)-16:y(i)+15 )
	    if max(ssim) lt maxl then begin
		res = gauss2dfit(ssim, a, /tilt)
		if a(2) gt 0 and a(3) gt 0 and $
		   a(2) lt 15.0 and a(3) lt 15.0 then begin
			see = (a(2) + a(3)) / 2.0
			fwhms(i) = see * 2.354
		endif
	    endif
	endif
endfor	; loop over stars
;
; compute grand fwhm
t=where(fwhms gt 0., nstar)
if nstar gt 0 then $
	ims,fwhms(t),fwhm,sig $
else	fwhm = 0.0
;
return
end	; pro get_fwhm2
