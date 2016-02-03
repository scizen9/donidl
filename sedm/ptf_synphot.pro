pro ptf_synphot,spf,smoo
;+
; synphot	- calculate synthetic photometry for PTF r and g bands
;-
common filter_info
;
Uj = 0
Bj = 1
Vj = 3
Rj = 4
Ij = 5
u = 6
g = 7
r = 8
i = 9
fids = [u,g,r,i,Uj,Bj,Vj,Rj,Ij]
;
; mags
mgs = fltarr(n_elements(fids)) -1.
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
readcol,spf,w,f,form='f,f',comment='#',/silent
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
print,'Observed Mags'
for k = 0,n_elements(fids)-1 do begin
    fi = fids[k]
    if master_filter[fi].mean_wave-450. gt w[0] then begin
	fwav = master_filter[fi].wave
	frsp = master_filter[fi].response
	g = where(fwav gt 0.0)
	if k ge 4 then begin
		oplot,fwav[g],frsp[g]*!y.crange[1],linesty=1
		fflux = filter_integ(fi,w,f,0.)/master_filter[fi].area_lambda
		mag = flux_to_apparent_mag(fflux,ifilter=fi,/vegasys)
		print,master_filter[fi].shortid+':',fflux, $
			' erg/cm^2/s, ',mag,' Vega mag', $
			format='(a15,g10.4,a,f9.3,a)'
	endif else begin
		oplot,fwav[g],frsp[g]*!y.crange[1],color=fcol[k]
		fflux = filter_integ(fi,w,f,0.)/master_filter[fi].abzp
		mag = flux_to_apparent_mag(fflux,ifilter=fi,/absys)
		xyouts,master_filter[fi].mean_wave-450.,-ydel*0.5, $
			master_filter[fi].shortid + ': ' + $
			strtrim(string(mag,form='(f9.3)'),2),charsi=1.5, $
			charthi=th
		print,master_filter[fi].shortid+':',fflux/1.e-26, $
			' mJy,        ',mag,' AB mag', $
			format='(a15,f10.4,a,f9.3,a)'
	endelse
	mgs[k] = mag
    endif else begin
	print,master_filter[fi].shortid+':','Not enough spectral coverage', $
		format='(a15,1x,a)'
    endelse
endfor
;
; Calculate Johnson mags
; Using Lupton (2005) transformations found here:
; https://www.sdss3.org/dr8/algorithms/sdssUBVRITransform.php#Lupton2005
Bmg = mgs[1] + 0.3130 * (mgs[1]-mgs[2]) + 0.2271
Vmg = mgs[1] - 0.5784 * (mgs[1]-mgs[2]) - 0.0038
Rmg = mgs[2] - 0.2936 * (mgs[2]-mgs[3]) - 0.1439
Img = mgs[2] - 1.2444 * (mgs[2]-mgs[3]) - 0.3820
;
; here we use Jordi et al. (2006) for U-B:
; https://www.sdss3.org/dr8/algorithms/sdssUBVRITransform.php#Jordi2006
if mgs[0] gt 0 then begin
	UmB = 0.79 * (mgs[0]-mgs[1]) - 0.93
	Umg = UmB + Bmg
endif else	Umg = -1.
;
; get fluxes
Bflx = flux_to_apparent_mag(Bmg,ifilter=1,/vegasys,/reverse)
Vflx = flux_to_apparent_mag(Vmg,ifilter=3,/vegasys,/reverse)
Rflx = flux_to_apparent_mag(Rmg,ifilter=4,/vegasys,/reverse)
Iflx = flux_to_apparent_mag(Img,ifilter=5,/vegasys,/reverse)
print,'Derived Vega Mags'
if Umg gt 0 then begin
	Uflx = flux_to_apparent_mag(Umg,ifilter=0,/vegasys,/reverse)
	print,'U:',Uflx,' erg/cm^2/s, ', Umg,' Vega mag', $
		format='(a15,g10.4,a,f9.3,a)'
endif
print,'B:',Bflx,' erg/cm^2/s, ', Bmg,' Vega mag', format='(a15,g10.4,a,f9.3,a)'
print,'V:',Vflx,' erg/cm^2/s, ', Vmg,' Vega mag', format='(a15,g10.4,a,f9.3,a)'
print,'R:',Rflx,' erg/cm^2/s, ', Rmg,' Vega mag', format='(a15,g10.4,a,f9.3,a)'
print,'I:',Iflx,' erg/cm^2/s, ', Img,' Vega mag', format='(a15,g10.4,a,f9.3,a)'
;
oplot,!x.crange,[0,0],thick=th
;
return
end
