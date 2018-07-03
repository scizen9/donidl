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
big = fltarr(nseg) - 1.
small = fltarr(nseg) - 1.
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
	if yrms[j] le 0.6 and yrms[j] ge 0.52 and costh > 0.7 and $
	   nx gt 9 then begin
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
			xx = xx[g]
			sg = sg[g]
			sgth = sg*costh
			coef = linfit(xx,sgth,/double,yfit=yfit)
			diff = sgth-yfit
			ims_asym,diff,md,sgd,wgt,siglim=[3.,2.]
			g = where(wgt eq 1)
			b = where(wgt ne 1,nb)
			;print,'nrej: '+strn(nb)
			;xx = xx[g]
			;sg = sg[g]
			coef = linfit(xx[g],sgth[g],/double)
			yfit = coef[0] + xx * coef[1]
			plot,xx,sg,psym=-5, title='Seg: '+strn(sno)
			oplot,xx,sgth,psym=-6
			oplot,xx,yfit,linesty=2,thick=5
			if nb gt 0 then oplot,xx[b],sgth[b],psym=2,symsi=2.
			;
			; store values
			mm = minmax(yfit)
			small[j] = mm[0]
			big[j] = mm[1]
			;read,'next: ',q
		endif else print,'No good points'
	endif
endfor
;
; clean
g = where(big gt 0, ng)
big = big[g]
small = small[g]
;
; final histo
plothist,big,bin=0.05,xran=[0.,1.5]
plothist,small,bin=0.05,/overplot,linesty=2
;
return
end
