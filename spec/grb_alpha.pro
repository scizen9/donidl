;+
; grb_alpha - fit power law to GRB spectrum
;
;-
pro grb_alpha, ifil, wave0, wave1, lambda=lambda, title=title, savefile=savefile
;
rdfits1dspec,ifil,wave,flux
;
; check inputs
if n_elements(wave0) ne 1 then wave0 = min(wave)
if n_elements(wave1) ne 1 then wave1 = max(wave)
;
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
	xlab = 'WAVELENGTH ($\AA$)'
	ylab = '$F_{\lambda}\ (erg s^{-1} cm^{-2} \AA^{-1})$'
	lpos = [0.9, 0.3]
; convert to nu and Fnu
endif else begin
	xall = 2.998e+18/wave
	yall = (wave^2/2.998e+18) * flux
	xdata = xall[fr]
	ydata = yall[fr]
	xlab = 'FREQUENCY (Hz)'
	ylab = '$F_{\nu}\ (erg s^{-1} cm^{-2} Hz^{-1})$'
	lpos = [0.4, 0.3]
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
xran = [min(xall), max(xall)]
p = plot( xall, yall, /ylog, /xlog, xtitle=xlab, ytitle=ylab, title=tlab, $
	yran=yran,xthick=th, ythick=th, font_size=12,/stairstep, $
	name='Data', xran=xran, /xstyle, /ystyle)
p2 = plot( xdata, 10.^y_fit, linesty=2, thick=5,/overplot, $
	name='$\alpha = $'+string(-res[1], form='(f5.3)'))
leg = legend(target=[p,p2], position=lpos)

if keyword_set(savefile) then begin
	p.save, savefile
	print,'saved plot to '+savefile
endif
;
print,'alpha = ', -res[1], format='(a, f7.3)'
end
