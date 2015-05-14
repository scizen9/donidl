pro pltqsotrends,lfile,expcut=expcut,sncut=sncut
;+
; pltqsos - plot photometry of qso's in list file
;-
flist=file_search(lfile,count=nf)
if nf ne 1 then begin
	print,'PLTQSOS: Error - file not found: ',lfile
	return
endif
;
readcol,lfile,fv,nv,fn,nn,famg,namg,ra,dec,z,loiiig,loiiinp, $
	format='f,f,i,i,f,f,a,a,f,f,f'
ntp=n_elements(fv)
;
; plot trends
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
!p.multi=[0,1,2]
th=3
si=1.5
cd,'./',current=cwd
sta=strsplit(cwd,'/',/extract)
dir=sta(n_elements(sta)-1)
tlab = dir + '   ' + lfile
xran=[-0.01,0.75]
data=[[z],[loiiinp],[namg],[famg],[float(nn)],[float(fn)],[nv],[fv]]
labs=['z', 'L(OIII)', 'NUV AV MAG', 'FUV AV MAG', 'NUV Nobs', 'FUV Nobs', $
	'NUV Fvar', 'FUV Fvar']
yrarr=[[0.,0.9],[6.2,10.7],[23.5,15.7],[23.5,15.7],[-5,250],[-5,250], $
	[-0.01,0.75],[-0.01,0.75]]
q=''
for i=0,n_elements(labs)-1 do begin
	xx=reform(data[*,i])
	yran=reform(yrarr[*,i])
	ylab=labs[i]
	g=where(fv ge 0. and xx gt 0.)
;	if strpos(ylab,'MAG') ge 0 then begin
;		yran=[max(xx(g)),min(xx(g))]
;		del=yran[0]-yran[1]
;		yran[0]=yran[0]+del/20.
;		yran[1]=yran[1]-del/20.
;	endif else begin
;		yran=[min(xx(g)),max(xx(g))]
;		del=yran[1]-yran[0]
;		yran[0]=yran[0]-del/20.
;		yran[1]=yran[1]+del/20.
;	endelse
plot,fv(g),xx(g),thick=th,xthick=th,ythick=th,charsi=si,charthi=th,/nodata, $
	xtitle='FUV Fvar', xran=xran, xsty=1, title=tlab, $
	ytitle=ylab, yran=yran, ysty=1
oplot,fv(g),xx(g),psym=5,symsi=si,thick=th,color=colordex('P')
g=where(nv ge 0. and xx gt 0.)
plot,nv(g),xx(g),thick=th,xthick=th,ythick=th,charsi=si,charthi=th,/nodata, $
	xtitle='NUV Fvar', xran=xran, xsty=1, $
	ytitle=ylab, yran=yran, ysty=1
oplot,nv(g),xx(g),psym=6,symsi=si,thick=th,color=colordex('B')
read,'next: ',q
endfor
!p.multi=0
;
;
return
end
