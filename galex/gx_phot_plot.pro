pro gx_phot_plot,fname,xrange=xrange,grism=grism
;
; read in and plot photometry
;
readcol,fname,frm,ecl,exptim,mdnuv,fuv,fuverr,nuv,nuverr, $
	ggoid,ra,dec,dis,fld,date, /silent, $
	format='i,l,f,d,f,f,f,f,l,d,d,f,a,a'
nrec = n_elements(exptim)
;
; check for good data
gf=where(fuv gt 0.,ngf)
gn=where(nuv gt 0.,ngn)
if ngf le 0 and ngn le 0 then begin
	print,'No good data points, returning'
	return
endif
;
; convert dates
mjd = dblarr(nrec)
for i=0,nrec-1 do begin
	dt = date(i)
	yr = gettok(dt,'-')
	mo = gettok(dt,'-')
	dy = gettok(dt,' ')
	hr = gettok(dt,':')
	mn = gettok(dt,':')
	sc = dt
	mjd(i) = julday(mo,dy,yr,hr,mn,sc)
endfor
mjd0 = long(min(mjd))
mjd = mjd - mjd0
pyr = indgen(10)+2003
pjd = julday(1,1,pyr) - mjd0
;
; set up plot
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=3
si=1.8
;
;
if ngf gt 0 and ngn gt 0 then begin
	ydel = max([fuv(gf),nuv(gn)]) - min([fuv(gf),nuv(gn)])
	yrng = [max([fuv(gf),nuv(gn)])+ydel*0.1,min([fuv(gf),nuv(gn)])-ydel*0.1]
endif else if ngf gt 0 and ngn le 0 then begin
	ydel = max(fuv(gf)) - min(fuv(gf))
	yrng = [max(fuv(gf))+ydel*0.1, min(fuv(gf))-ydel*0.1]
endif else begin
	ydel = max(nuv(gn)) - min(nuv(gn))
	yrng = [max(nuv(gn))+ydel*0.1,min(nuv(gn))-ydel*0.1]
endelse
;
if keyword_set(xrange) then $
	xrng = xrange $
else	begin
	xdel = max(mjd) - min(mjd)
	xrng = [min(mjd)-xdel*0.1, max(mjd)+xdel*0.1]
endelse
;
plot,[0,0],[1,1],thick=th,xthick=th,ythick=th, $
	charsi=si,charthi=th,title=fname, $
	xtitle='JD - '+strtrim(string(mjd0),2), xrange=xrng, xsty=1, $
	ytitle='MAG(AB)',yrange=yrng,ysty=1,/nodata
if ngn gt 0 then begin
	oplot,mjd(gn),nuv(gn),thick=th,psym=5,color=colordex('red')
	oploterror,mjd(gn),nuv(gn),nuverr(gn),psym=3
endif
if ngf gt 0 then begin
	oplot,mjd(gf),fuv(gf),thick=th,psym=4,color=colordex('blue')
	oploterror,mjd(gf),fuv(gf),fuverr(gf),psym=3
endif
legend,['FUV','NUV'],psym=[4,5],thick=[th,th],charsi=si,charthi=th, $
	color=[colordex('blue'),colordex('red')]
for i=0,n_elements(pjd)-1 do begin
	if pjd[i] gt xrng[0] and pjd[i] lt xrng[1] then begin
		oplot,[pjd[i],pjd[i]],yrng,linesty=2
		xyouts,pjd[i],yrng[0]-ydel*0.025,strn(pyr[i]),orient=90.
	endif
endfor
;
; check for grism obs if requested
if keyword_set(grism) then begin
	flist = file_search('g/*-img/*ng-gsax.fits',count=nf)
	if nf gt 0 then begin
	    lab = strarr(nf)
	    for i=0,nf-1 do begin
		hdr=headfits(flist(i))
		obd = sxpar(hdr,'OBS-DATE')
		obt = sxpar(hdr,'TIME-OBS')
		yr = gettok(obd,'-')
		mo = gettok(obd,'-')
		dy = obd
		hr = gettok(obt,':')
		mn = gettok(obt,':')
		sc = obt
		gmjd = julday(mo,dy,yr,hr,mn,sc) - mjd0
		oplot,[gmjd,gmjd],yrng,linesty=i
		lab(i) = gettok(flist(i),'-')
	    endfor
	    legend,lab,linesty=indgen(nf),/right,/bottom,charsi=si,charthi=th
	endif else print,'No grism data found'
endif
;
return
end
