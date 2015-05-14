pro pltnqobs
;+
; pltnqobs - plot a histogram of the number of observations of a qso
;
; 	looks for files *.pht
;-
flist=file_search('*.pht', count=nf)
if nf lt 1 then begin
	print,'PLTNQOBS: Error - no *.pht files found'
	return
endif
;
; check for NO COVERAGE
rec=''
for i=0,nf-1 do begin
	openr,il,flist(i),/get_lun
	for j=0,1 do readf,il,rec
	if strpos(rec,'NO') ge 0 then flist(i) = ''
	free_lun,il
endfor
good=where(strlen(flist) gt 0, ngood)
if ngood gt 0 then begin
	flist=flist(good)
	nf = ngood
endif else begin
	print,'PLTNQOBS: Error - no *.pht file with coverage'
	return
endelse
;
; get number of real obs in each file
nobs=lonarr(nf)
fobs=lonarr(nf)
for i=0,nf-1 do begin
	rdqsophot,flist(i),ra,dec,dis,fm,fme,nm,nme
	gf=where(fm(*,3) gt 0., ngf)
	gn=where(nm(*,3) gt 0., ngn)
	nobs(i)=ngn
	fobs(i)=ngf
endfor
;
cd,'./',current=cwd
sta=strsplit(cwd,'/',/extract)
dir=sta(n_elements(sta)-1)
;
hn=histogram(nobs,min=1,loc=xnh)
hf=histogram(fobs,min=1,loc=xfh)
;
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=3
si=1.5
;
plot,xnh,hn,thick=th,charsi=si, charthi=th, xthick=th, ythick=th, xcharsi=si, $
	ycharsi=si, title=dir, psym=10, $
	xtitle='N obs', xran=[0.9,max(nobs)], xsty=1, /xlog, $
	ytitle='N qso', yran=[-(max([hn,hf])/20.), max([hn,hf])*1.05], ysty=1
oplot,xfh,hf,thick=th,color=colordex('P'),psym=10
oplot,xnh,hn,thick=th,color=colordex('B'),psym=10
legend,['FUV: '+strtrim(long(total(hf)),2),'NUV: '+strtrim(long(total(hn)),2)],$
	thick=[th,th],color=[colordex('P'),colordex('B')], linesty=[0,0], $
	charsi=si*2.0,charthi=th,/right,box=0
;xyouts,max(xh)*0.02,max(h)*0.85,'TOTAL NOBS: ' + $
;	strtrim(string(long(total(h))),2), $
;	charthi=th,charsi=si*2.0
;
return
end
