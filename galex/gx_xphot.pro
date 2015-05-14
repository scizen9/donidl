pro gx_xphot,xfil,delt,time,ctsz,ctsc,cts,xdet=xdet,ydet=ydet,check=check, $
	ps=ps,ra=ra,dec=dec,epoch=epoch,verbose=verbose
;+
; gx_xphot - get photometry from mchx file
;
; INPUTS:
;	xfil - filename of mchx file
;	delt - time sampling in seconds
;
; OUTPUTS:
;	time - time in seconds since 0UTC 1980-JAN-06 ("GPS time")
;		for midpoint of time bin.
;	ctsz - counts/sec for cleaned and de-zeroed photon events
;	ctsc - counts/sec for cleaned photon events (no skip flag).
;	cts  - counts/sec for all photon events.
;	epoch- epoch of first second: MMM DD HH:MM:SS YYYY
;
; KEYWORDS:
;	xdet, ydet - output variables for detector x,y
;	check - plot diagnostics to see if variations correlate with anything
;	ps - make a hardcopy of check plot
;
; HISTORY:
;	15-OCT-2010 - Initial version, neill@srl.caltech.edu
;	19-OCT-2010 - removed file output
;	02-AUG-2011 - added epoch keyword
;-
	hdr0 = headfits(xfil,/silent)
	ra   = sxpar(hdr0,'OBRA000')
	dec  = sxpar(hdr0,'OBDEC000')
	epoch= systime(0,sxpar(hdr0,'ASPT0') + 315964800d0,/utc)
	jnk  = gettok(epoch,' ')	; trim off day of week

	mchx = mrdfits(xfil,1,hdr,/silent)

	cln  = mchx[where((mchx.flags and 4) ne 4)]
	all  = mchx[where((mchx.flags and 36) ne 36)]
	
	ti = mchx[0].t
	tf = mchx[n_elements(mchx)-1L].t
	span = tf - ti
	np = fix(span/delt) - 1l	; skip last interval
	if keyword_set(verbose) then $
		print,'Nsam,Ti,Tf,DelT: ',np,ti,tf,span, $
		form='(a,2x,i9,2f15.4,f9.3)'
	cts = fltarr(np)
	ctsc = fltarr(np)
	time = dblarr(np)
	xdet = fltarr(np)
	ydet = fltarr(np)
	t0 = ti + delt	; skip first interval
	t1 = t0 + delt

	for i=0l,np-1l do begin
		c = where(cln.t ge t0 and cln.t lt t1, nc)
		a = where(all.t ge t0 and all.t lt t1, na)
		if nc gt 0 then begin
			xdet[i] = total(cln[c].thetax)/float(nc)
			ydet[i] = total(cln[c].thetay)/float(nc)
			ctsc[i] = float(nc) / delt
		endif
		if na gt 0 then cts[i]  = float(na) / delt
		time[i] = t0 + delt * 0.5
		t0 = t1
		t1 = t0 + delt
	endfor

	z = where(ctsc le 0., nz)	; find zeros
	if nz gt 0 then begin
		zp= z + 1L			; grow by +1
		ctsz = ctsc
		ctsz[zp] = 0.
	endif else ctsz = ctsc
	;
	if keyword_set(check) then begin
		font_store = !p.font
		if keyword_set(ps) then begin
			temp = xfil
			rute = gettok(temp,'.')
			psfile,rute+'_chk'
			!p.font = 0
		endif
		deepcolor
		!p.background=colordex('white')
		!p.color=colordex('black')
		th=3
		si=0.85
		;
		!p.multi=[0,2,2]
		plot,xdet/10.,ydet/10.,thick=th,charsi=si,charthi=th, $
			xthick=th,xtitle='Theta X (asec)', xsty=1, $
			ythick=th,ytitle='Theta Y (asec)', ysty=1, $
			title=xfil
		resx = fft(xdet,-1)
		fx = findgen(n_elements(xdet)/2+1)/(n_elements(xdet)*delt)
		resy = fft(ydet,-1)
		fy = findgen(n_elements(ydet)/2+1)/(n_elements(ydet)*delt)
		yrng = [min([abs(resx),abs(resy)]),max([abs(resx),abs(resy)])]
		xrng = [1.e-4,1./delt]
		plot,fx,abs(resx),/xlog,/ylog,thick=th,charsi=si,charthi=th, $
			xthick=th,xtitle='Frequency (Hz)',xran=xrng,xsty=1, $
			ythick=th,ytitle='Power',yran=yrng,ysty=1, $
			linesty=1
		oplot,fy,abs(resy),thick=th,linesty=2
		legend,['Theta X','Theta Y'],linesty=[1,2],thick=[th,th], $
			charsi=si,charthi=th,box=0,/right
		ydel = max(ctsz) - min(ctsz)
		yrng = [min(ctsz) - ydel*0.1, max(ctsz) + ydel*0.1]
		sx = sort(xdet)
		xds = xdet[sx]/10.
		csx = ctsz[sx]
		plot,xds,csx,psym=1,thick=th,charsi=si,charthi=th, $
			xthick=th,xtitle='Theta X (asec)', xsty=1,symsi=0.5, $
			ythick=th,ytitle='Counts/sec',yran=yrng,ysty=1
		oplot,xds,smooth(csx,15),thick=th,color=colordex('R')
		sy = sort(ydet)
		yds = ydet[sy]/10.
		csy = ctsz[sy]
		plot,yds,csy,psym=1,thick=th,charsi=si,charthi=th, $
			xthick=th,xtitle='Theta Y (asec)', xsty=1,symsi=0.5, $
			ythick=th,ytitle='Counts/sec',yran=yrng,ysty=1
		oplot,yds,smooth(csy,15),thick=th,color=colordex('R')
		!p.multi=0
		if keyword_set(ps) then psclose
		!p.font = font_store
	endif
	return
end
