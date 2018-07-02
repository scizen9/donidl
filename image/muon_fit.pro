pro muon_fit, imf, gplot=gplot
;+
;  muon_fit - fit muons in the dark image
;-
;
im = mrdfits(imf, 0, imhdr)
seg = mrdfits(strmid(imf, 0, strpos(imf,'.'))+'_seg.fits', 0, sghdr)
readcol,strmid(imf, 0, strpos(imf,'.'))+'_cr.cat', $
	sids, xs, ys, xrms, yrms, thetas, format='i,f,f,f,f,f', $
	comment='#'
;
; convert thetas to radians
thetas = thetas * !pi / 180.
;
nseg = n_elements(sids)
q=''
;
for j = 0, nseg-1 do begin
	;
	; get ID
	sno = sids[j]
	;
	; get costheta
	costh = cos(thetas[j])
	;
	; get x points
	t = where(seg eq sno)
	ind = array_indices(seg, t)
	xs = minmax(ind[0,*])
	nx = (xs[1] - xs[0]) + 1
	;
	; get larger muons within a certain angle
	if yrms[j] le 0.6 and yrms[j] ge 0.52 and costh > 0.5 and nx gt 7 then begin
		xx = indgen(nx) + xs[0]
		sg = fltarr(nx)
		;
		; loop over x points
		for i = 0, nx-1 do begin
			;
			; fit CR
			sig = muon_sample(im, seg, sno, xx[i], gplot=gplot)
			;
			; store results
			sg[i] = sig
		endfor
		g = where(xx gt 0 and sg gt 0., ng)
		if ng gt 0 then begin
			plot,xx[g],sg[g],psym=-5, title='Seg: '+strn(sno)
			oplot,xx[g],sg[g]*costh,psym=-6
			read,'next: ',q
		endif else print,'No good points'
	endif
endfor
;
return
end
