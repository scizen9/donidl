pro ptf_synphot,spf,smoo
;+
; synphot	- calculate synthetic photometry for PTF r and g bands
;-
common filter_info
;
u = 6
g = 7
r = 8
i = 9
fids = [u,g,r,i]
;
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
si=1.75
th=3
;
fcol = [colordex('Blue'), colordex('Green'), $
	colordex('Orange'), colordex('Red')]
;
readcol,spf,w,f,form='f,f',comment='#'
;
; smooth if requested
if n_params(0) ge 2 then $
	f = smooth(f,smoo)
;
t=where(w gt 4000 and w lt 9000, nt)
if nt gt 0 then begin
	ylims = get_plotlims(f[t])
	ydel = ylims[1]*0.1
	ylims[0] = -ydel
	xlims = [3200., 9100.]
endif else begin
	print,'No data in range 4000 - 9000 Angstroms, returning'
	return
endelse
plot,w,f,title=spf,charsi=si,charthi=th, $
	xran=xlims,/xs,xthick=th,xtitle='Wavelength(A)', $
	yran=ylims,/ys,ythick=th,ytitle='Flux/Response',/nodata
oplot,w,f,thick=th

;
; loop over filters
for k = 0,n_elements(fids)-1 do begin
    fi = fids[k]
    if master_filter[fi].mean_wave-450. gt w[0] then begin
	oplot,master_filter[fi].wave,master_filter[fi].response*!y.crange[1], $
		color=fcol[k]
	fflux = filter_integ(fi,w,f,0.)/master_filter[fi].abzp
	mag = flux_to_apparent_mag(fflux,ifilter=fi,/absys)
	xyouts,master_filter[fi].mean_wave-450.,-ydel*0.5, $
		master_filter[fi].shortid + ': ' + $
		strtrim(string(mag,form='(f9.3)'),2),charsi=1.5,charthi=th

	print,master_filter[fi].shortid+':',fflux/1.e-26,' mJy, ', $
		mag,' AB mag', format='(a15,f10.4,a,f9.3,a)'
    endif else begin
	print,master_filter[fi].shortid+':','Not enough spectral coverage', $
		format='(a15,1x,a)'
    endelse
endfor
;
oplot,!x.crange,[0,0],thick=th
;
return
end
