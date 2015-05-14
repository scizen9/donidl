pro get_fwhm,im,fwhm,maxlim=maxlim,nsigma=nsigma,background=background
;+
; get_fwhm - get the fwhm of stars in IM
;-
; check keywords
if keyword_set(maxlim) then $
	maxl = maxlim $
else	maxl = max(im)
if keyword_set(nsigma) then $
	nsig = nsigma $
else	nsig = 5
;
; get dimensions of image
s = size(im)
nx = s(1) & ny = s(2)
;
; get samples
nxsam = fix(nx / 512) - 1
nysam = fix(ny / 512) - 1
ntsam = nxsam * nysam
background = 0.
;
; init average fwhm
fwhm = 0.
nf = 0L
;
; loop over samples
for iy = 0, nysam-1 do begin
	y0 = (iy * 256) + 128
	for ix = 0, nxsam-1 do begin
		x0 = (ix * 256) + 128
		sim = im( x0-127:x0+128, y0-127:y0+128 )
;
; get sky values
		sky,sim,skymode,skysig,/silent
		background = background + skymode
		hmin = skymode + nsig * skysig
		nstar = myfind(sim,x,y,flux,sharp,round,hmin,10.0, $
			[-0.8,0.8],[0.2,1.0],/silent)
		;print,'ix,iy, x0,y0, nstar: ',ix,iy,x0,y0,nstar
		for i = 0,nstar-1 do begin
			if x(i) gt 16 and x(i) lt 239 and $
			   y(i) gt 16 and y(i) lt 239 and $
			   sim( fix(x(i)+0.5),fix(y(i)+0.5) ) lt maxl then $
			begin
				ssim = sim( x(i)-16:x(i)+15, y(i)-16:y(i)+15 )
				res = gauss2dfit(ssim, a, /tilt)
				if a(2) gt 0 and a(3) gt 0 and $
				   a(2) lt 15.0 and a(3) lt 15.0 then begin
					see = (a(2) + a(3)) / 2.0
					fwhm = fwhm + see * 2.354 ; sig 2 fwhm
					nf = nf + 1L
				endif
			endif
		endfor	; loop over stars
	endfor	; loop over x samples
endfor	; loop over y samples
;
; compute grand fwhm
fwhm = fwhm / float(nf)
;
; compute grand background
background = background / ntsam
;
return
end	; pro get_fwhm
