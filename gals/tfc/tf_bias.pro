pro tf_bias, zp, sl, cv, rms, nsimulations=nsimulations, ps=ps
;+
;	tf_bias - perform bias experiment for TFR coeffs and RMS given
;
; INPUTS:
;	zp,sl,cv	- coeffs of TFR (cv = 0 for linear)
;	rms		- observed scatter in magnitudes about TFR
;
; KEYWORDS:
;	nsimulations	- number of simulations to perform (defaults to 1000)
;
; HISTORY:
;	2014-MAY-21	Initial Revision, D. Neill
;-
; number of simulations
if keyword_set(nsimulations) then $
	nsim = nsimulations $
else	nsim = 1000
;
; bias samples
nsam = 12
bias = fltarr(nsam,nsim)
mlim = -17.0 - findgen(nsam) * 0.5
arms = 0.
;
; loop over simulations
for j=0,nsim-1 do begin
	tf_sim,zp,sl,cv,rms,smag,slw,delmag
	;
	; record rms
	arms = arms + stddev(delmag)
	;
	; now loop over bias samples
	for i=0,nsam-1 do begin
		;
		; sample mag limit
		use = where(smag le mlim[i], nuse)
		if nuse gt 0 then $
			bias[i,j] = total(delmag[use])/float(nuse)
	endfor
endfor
;
arms = arms / float(nsim)
print,' '
print,'Avg Simulated RMS (mag): ',arms,form='(a,f5.3)'
;
; process bias
mbias = fltarr(nsam)
mbsig = mbias
for i=0,nsam-1 do begin
	if nsim gt 1 then begin
		mo = moment(bias[i,*])
		mbias[i] = mo[0]
		mbsig[i] = sqrt(mo[1])
	endif else begin
		mbias[i] = bias[i,0]
		mbsig[i] = 0.01
	endelse
endfor
mbias = mbias - min(mbias)
if keyword_set(ps) then begin
	psfile,'bias'
	!p.font=0
endif
;
; set up plot
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=3
si=1.5
mu = textoidl('\mu')
chi = textoidl('\chi^2_{\nu}')
;
; fit bias
x = 14.5+abs(mlim) - 31.
res = mpfitfun('biasfunc',x,mbias,mbsig/sqrt(nsim),[0.005,2.0], $
	bestnorm=chisq, perror=sigma,/quiet)
rstr = ['bias = '+string(res[0],'(f6.3)')+'('+mu+'-31)!U'+ $
	string(res[1],'(f4.2)')+'!N', $
	chi + ' = '+string(chisq/10.,'(f6.1)') ]
print,' '
print,'bias      = '+string(res[0],'(f6.3)')+'(mu-31)^'+ $
	string(res[1],'(f4.2)')
print,'chi^2/DOF = ',chisq/10.
;
vsym,24,/fill
ylim = get_plotlims(mbias,mbsig)
plot,mlim,mbias,psym=8,thick=th,charsi=si,charthi=th,symsi=si, $
	xtitle='Magnitude Limit',xthick=th,xran=[-16.5,-23],/xs, $
	ytitle='Modulus Bias',ythick=th,yran=ylim,/ys
oploterror,mlim,mbias,(mlim-mlim),mbsig,psym=3
oplot,!x.crange,[0,0],linesty=5,thick=th
xx = reverse(findgen(950)/100.) - 24.5
pb = res[0] * ( (14.5+abs(xx)) - 31.)^res[1]
oplot,xx,pb,color=colordex('red'),thick=th,linesty=2
y0 = !y.crange[1] - (!y.crange[1] - !y.crange[0])*0.056
legend,rstr,box=0,charsi=si,charthi=th,spac=2.5,pos=[-18.7,y0]
;
leg = ['ZP = '+string(zp,'(f6.2)'), 'Slope = '+string(sl,'(f6.2)')]
if cv ne 0. then $
	leg = [leg, 'Curve = '+string(cv,'(f5.2)')]
leg = [leg, 'RMS = '+string(rms,'(f4.2)'),'Nsim = '+strn(nsim)]
legend,leg,box=0,charsi=si,charth=th,spac=2.5
;
if keyword_set(ps) then $
	psclose
;
return
end
