;+
; grb_alpha - fit power law to GRB spectrum
;
;-
pro grb_alpha, ifil, wave0, wave1, lambda=lambda, title=title
;
rdfits1dspec,ifil,wave,flux
fr = where(wave gt wave0 and wave lt wave1, nfit)
print,'fitting ', nfit, ' points'
if nfit le 3 then begin
	print,'Too few points for meaningful fit!'
	return
endif
if keyword_set(title) then $
	tlab = title $
else	tlab = ifil
;
; use lambda and Flambda
if keyword_set(lambda) then begin
	xall = wave
	yall = flux
	xdata = wave[fr]
	ydata = flux[fr]
	xlab = 'WAVELENGTH (A)'
	ylab = 'Flambda (erg/s/cm^2/A)'
; convert to nu and Fnu
endif else begin
	xall = 2.998e+18/wave
	yall = (wave^2/2.998e+18) * flux
	xdata = xall[fr]
	ydata = yall[fr]
	xlab = 'FREQUENCY (Hz)'
	ylab = 'Fnu (erg/s/cm^2/Hz)'
endelse
;
; power-law fit
res = linfit(alog10(xdata), alog10(ydata), yfit=y_fit)
;
; plot
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
si=1.75
th=3
;
; get y limits for plot
pf = 0.8
yran = [-1, 0]
while yran[0] lt 0 do begin
	yran = get_plotlims(10.^y_fit, pad_fraction=pf)
	if yran[0] lt 0 then pf -= 0.1
endwhile
print,pf,yran
;
plot, xall, yall, /ylog, /xlog, /xs, xtitle=xlab, ytitle=ylab, title=tlab, $
	yran=yran,/ys, xthick=th, ythick=th, charsi=si, charthi=th, psym=10
oplot, xdata, 10.^y_fit, linesty=2, thick=5

;
legend,['Data', 'alpha = '+string(-res[1], form='(f5.3)')], linesty=[0,2], $
	thick=[0, 5],/left,/bottom, charsi=si, charthi=th
;
print,'alpha = ', -res[1], format='(a, f7.3)'
end
