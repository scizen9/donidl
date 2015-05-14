pro tf_sim, zp, sl, cv, rms, smag, slw, delmag, display=display, ps=ps
;+
;	tf_sim - simulate a well-sampled TFR with RMS noise obeying
;		a Schecter function with alpha = -1. and Mstar = -22.
;
; INPUTS:
;	zp,sl,cv	- TFR coeffs (cv = 0 for linear)
;	rms		- Observed scatter about TFR
;
; OUTPUTS:
;	smag		- simulated absolute magnitudes
;	slw		- simulated log corrected linewidths
;
; KEYWORDS:
;	display		- set to produce diagnostic plots
;
; HISTORY:
;	2014-MAY-21	Initial Revision, D. Neill
;
;-
; generate a probability distribution from the
; Schecter function
alpha = -1.
mstar = -22.
phistar = 1.085
mglim = -24.5
mag = reverse(findgen(950)/100.) + mglim
prob = schecter(alpha,mstar,phistar,mag)
;
; plot probability distribution
if keyword_set(display) and not keyword_set(ps) then begin
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	th=3
	si=1.5
	plot,mag,prob,charsi=si,charthi=th, $
		xran=[-15,-26],/xs,xtitle='Abs Mag',xthick=th, $
		yran=[0.,1.1],/ys,ytitle='Schecter Prob',ythick=th
	oplot,!x.crange,[1.,1.],linesty=5,thick=th
	oplot,[mstar,mstar],!y.crange,linesty=1
	q=''
	read,'Next: ',q
endif
;
; now pull 1000 galaxies out
ngal = 1000
n = 0
smag = [0.]
while n lt ngal do begin
	;
	; pick random magnitude in range
	rmag = randomu(seed) * 9.5 + mglim
	;
	; pick a probability for that galaxy
	pick = randomu(seed2)
	;
	; find probability associated with rmag
	t = where(mag le rmag, nt)
	if nt gt 0 then begin
		if pick le prob[t[0]] then begin
			smag = [ smag, rmag ]
			n = n + 1
		endif
	endif
endwhile
smag = smag[1:*]
;
; get simulated linewidths
if cv eq 0. then begin	; linear
	isl = 1./sl
	izp = 2.5 - zp * isl
	;
	; now make TFR
	slw = izp + isl * smag
endif else begin	; curved
	slw = -sl/(2.*cv) - sqrt( sl^2 - 4.*cv*(zp-smag) ) / $
		(2.*cv) + 2.5
endelse
;
; project rms onto linewidth axis
lrms = abs(rms/sl)
;
; now add noise
for i=0,ngal-1 do $
	smag[i] = smag[i] + randomn(seed3)*rms
	;slw[i] = slw[i] + randomn(seed3)*lrms
;
pmag = zp + sl * (slw-2.5) + cv * (slw-2.5)^2
delmag = pmag - smag
;
; check display keyword
if keyword_set(display) or keyword_set(ps) then begin
	if keyword_set(ps) then begin
		psfile,'tf_sim'
		fontstore = !p.font
		!p.font=0
	endif
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	th=3
	si=1.5
	vsym,24,/fill
	plot,slw,smag,psym=8,charsi=si,charthi=th, $
		xran=[1.7,3.0],/xs,xtitle='LogLinewidth',xthick=th, $
		yran=[-15.5,-26],/ys,ytitle='Abs Mag',ythick=th
	xx = findgen(1000)/500. + !x.crange[0]
	yy = zp+sl*(xx-2.5)+cv*(xx-2.5)^2
	oplot,xx,yy,thick=th*2,color=colordex('red')
	leg = ['ZP = '+string(zp,'(f6.2)'), 'Slope = '+string(sl,'(f6.2)')]
	if cv ne 0. then $
		leg = [leg, 'Curve = '+string(cv,'(f5.2)')]
	leg = [leg, 'RMS = '+string(rms,'(f4.2)')]
	legend,leg,box=0,charsi=si,charth=th,spac=2.5
	print,'Simulated RMS (mag): ',stddev(delmag),form='(a,f5.3)'
	if keyword_set(ps) then begin
		psclose
		!p.font=fontstore
	endif
endif
;
return
end
