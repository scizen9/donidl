pro specplot, ifil, wavelength_limits=wavelength_limits, $
	smooth_length=smooth_length, sigma_thresh=sigma_thresh, ps=ps
;+
; plot a spectrum
;
; INPUTS:
;	ifil - filename of ascii spectrum with wl flux columns
;+
readcol, ifil, wl, fl, format='f,f', comment='#'
;
if keyword_set(wavelength_limits) then $
	wlims = wavelength_limits $
else	wlims = [min(wl), max(wl)]

if keyword_set(smooth_length) then $
	flsm = smooth(fl, smooth_length) $
else	flsm = fl

if keyword_set(sigma_thresh) then $
	sigth = sigma_thresh $
else	sigth = 0.
;
; flux limits
pli = where(wl ge wlims[0] and wl le wlims[1], npli)
ylims = get_plotlims(flsm[pli], pad_fraction=0.10)
flsm = flsm[pli]
wl = wl[pli]
;
; lines
mo = moment(flsm)
ath = mo[0] + sqrt(mo[1]) * sigth
print,'line threshhold = ', ath
lines = findpeaks(wl, flsm, smooth_length, 0.001, ath, smooth_length)
;lines = clnpeaks( wl, flsm, smooth_length, 3*smooth_length, 2.)
;pks = isopeaks(flsm, smooth_length*3., level=ath)
;lines = wl[pks]
;pkexplore, wl, flsm, smooth_length, 0.001, ath, smooth_length
;
font_store = !p.font
if keyword_set(ps) then begin
	tmp = ifil
	pfile = gettok(tmp, '.')
	psfile,pfile
	th=7
	si=1.5
	!p.font=0
endif else begin
	th=3
	si=1.7
endelse

deepcolor
!p.background=colordex('white')
!p.color=colordex('black')

plot,wl, flsm, title=ifil, charsi=si, charthick=th, $
	xthick=th, xran=wlims, /xs, xtitle='WAVELENGTH (A)', $
	ythick=th, yran=ylims, /ys, ytitle='FLUX'
;oplot,!x.crange, [ath, ath], linesty=2

y0 = (!y.crange[1]-!y.crange[0])*0.075 + !y.crange[0]
;
; overplot lines
for i=0, n_elements(lines)-1 do begin
	xyouts,lines[i],y0,string(lines[i],format='(f6.1)'),ori=90,align=0.5
	;print,lines[i]
	;oplot,[lines[i], lines[i]], !y.crange, linesty=2
endfor

if keyword_set(ps) then psclose
!p.font=font_store
;
return
end
