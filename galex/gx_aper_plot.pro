pro gx_aper_plot,fname,xrange=xrange,note=note
;
; read in and plot photometry
;
readcol,fname,usec,mg,me,form='d,f,f',/silent
nrec = n_elements(usec)
;
; check for good data
g=where(mg gt 0. and me gt 0.,ng)
if ng le 0 then begin
	print,'No good data points, returning'
	return
endif
;
; convert dates
mjd = dblarr(nrec)
for i=0,nrec-1 do mjd[i] = unixjd(usec[i])
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
ydel = (max(mg[g]) - min(mg[g]))>0.5
yavg = avg(mg[g])
yrng = [max([yavg+ydel*0.5,max(mg[g])+ydel*0.1]), $
	min([yavg-ydel*0.5,min(mg[g])-ydel*0.1])]
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
oplot,mjd(g),mg(g),thick=th,psym=5,color=colordex('red')
oploterror,mjd(g),mg(g),me(g),psym=3
if keyword_set(note) then $
	legend,[note,''],charsi=si,charthi=th,box=0
;
for i=0,n_elements(pjd)-1 do begin
	if pjd[i] gt xrng[0] and pjd[i] lt xrng[1] then begin
		oplot,[pjd[i],pjd[i]],yrng,linesty=2
		xyouts,pjd[i],yrng[0]-ydel*0.025,strn(pyr[i]),orient=90.
	endif
endfor
;
return
end
