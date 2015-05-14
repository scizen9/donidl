pro gx_ptf_plan,jd0,jd1
;+
;
;-
pdir = '/home/galex/fltops/mps/ref/use/'
saaf = pdir + '2003_04_28.pass.stf'
misf = pdir + 'mission-etf.fits'
;
readcol,saaf,pins,pind,pous,poud,durs,form='a,d,a,d,f'
;
ms = mrdfits(misf,1,hdr)
;
if n_params(0) lt 2 then begin
	jd0 = 2455973.539246030d0
	jd1 = 2456027.894838260d0
	; to get 571 orbits, we add 5 days to jd1
	jd1 = jd1 + 5.d0
endif
t = where(ms.start_jd ge jd0 and ms.end_jd le jd1, nt)
saa = intarr(nt)
;
ext = fltarr(nt)
nmb = intarr(nt)
stday = ''
day0 = 0.d0
dran = 0.d0
;
; loop over eclipses
for i=0L,nt-1 do begin
	p = t[i]
	b = where( (ms[p].start_jd le pind and ms[p].end_jd ge pind) or $
		   (ms[p].start_jd le poud and ms[p].end_jd ge poud), nb)
	if nb gt 0 then begin
		saa[i] = 1
		print,i,ms[p].eclipse_num,' ',ms[p].start_ccsds
		ext[i] = -1.
		nmb[i] = -1
	endif else begin
		ext[i] = (ms[p].dur_secs - 390.0 - 30.*15.)/15.0
		nmb[i] = (ms[p].dur_secs - 390.0)/130.0
		if strlen(stday) le 0 then begin
			stday = ms[p].start_ccsds
			day0 = ms[p].start_jd
		endif
		fiday = ms[p].end_ccsds
		dran = ms[p].end_jd - day0
	endelse
endfor
;
good = where(saa eq 0, ngood)
print,'Start date     : ',stday
print,'End   date     : ',fiday
print,'Number of days : ',dran
print,'Total Orbits   : ',nt
print,'Orbits/day     : ',nt/dran
print,'Good Orbits    : ',ngood
print,'Good Orbits/day: ',ngood/dran
print,'<exp>15legs(s) : ',avg(ext[good])
print,'<nleg>100s     : ',avg(nmb[good])
;
deepcolor
!p.background=colordex('black')
!p.color=colordex('white')
!p.multi=[0,1,2]
plothist,ext[good],xtitle='15 LEG EXPO s/tile', $
	charsi=1.5,charthi=2.,/box,/halfbin
plothist,nmb[good],xtitle='Number of Tiles @ 100s/tile', $
	charsi=1.5,charthi=2.,/box,/halfbin
;
return
end
