pro qsovars,list=list,sncut=sncut,expcut=expcut
;+
; qsovars - plot a histogram of the variability
;
; 	looks for files *.pht
;-
flist=file_search('*.pht', count=nf)
if nf lt 1 then begin
	print,'QSOVARS: Error - no *.pht files found'
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
	print,'QSOVARS: Error - no *.pht file with coverage'
	return
endelse
;
; open list file if requested
if keyword_set(list) then begin
	if strlen(list) le 0 then $
		ofil='qsovars.list' $
	else	ofil=list
	filestamp,ofil
	openw,ol,ofil,/get_lun
	printf,ol,'# QSOVARS: '+systime(0)
endif
;
; get variability in each file
nfv =fltarr(nf)
ffv =fltarr(nf)
nnf =intarr(nf)
nnn =intarr(nf)
for i=0,nf-1 do begin
	if keyword_set(sncut) then $
		if keyword_set(expcut) then $
			fvar=qsofvar(flist(i), hline=hdr,nobs=nobs,avmg=avmg, $
				sncut=sncut,expcut=expcut) $
		else	fvar=qsofvar(flist(i), hline=hdr,nobs=nobs,avmg=avmg, $
				sncut=sncut) $
	else 	if keyword_set(expcut) then $
			fvar=qsofvar(flist(i),hline=hdr,nobs=nobs,avmg=avmg, $
				expcut=expcut) $
		else	fvar=qsofvar(flist(i),hline=hdr,nobs=nobs,avmg=avmg)
	nfv(i) = fvar[1]
	ffv(i) = fvar[0]
	nnf(i) = nobs[0]
	nnn(i) = nobs[1]
	if keyword_set(list) and (fvar[0] ge 0. or fvar[1] ge 0.) then $
		printf,ol,fvar,nobs,avmg,hdr, $
		form='(2f9.3,2i5,2f9.3,2x,a)'
endfor
;
cd,'./',current=cwd
sta=strsplit(cwd,'/',/extract)
dir=sta(n_elements(sta)-1)
if keyword_set(sncut) then $
	tlab = dir + '   S/N cut: '+string(sncut,form='(f4.1)') $
else	tlab = dir + '   NO S/N cut'
if keyword_set(expcut) then $
	tlab = tlab + '   EXPTIME cut: '+string(expcut,form='(f5.1)')+' s' $
else	tlab = tlab + '   NO EXPTIME cut'
;
ng = where(nfv ge 0, nng)
fg = where(ffv ge 0, nfg)
hn=histogram(nfv(ng),min=-0.1,max=0.75,bins=0.05,loc=xnh)
hf=histogram(ffv(fg),min=-0.1,max=0.75,bins=0.05,loc=xfh)
;
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=3
si=1.5
;
plot,xnh,hn,thick=th,charsi=si, charthi=th, xthick=th, ythick=th, $
	xcharsi=si, ycharsi=si, title=tlab, /nodata, $
	xtitle='Fvar', xran=[-0.05,0.75], xsty=1, $ ;xran=[0,max(xnh)], $
	ytitle='N qso', yran=[-0.05,1.05],ysty=1 ;yran=[-(max([hn,hf])/20.), max([hn,hf])*1.05], ysty=1
oplot,xfh,hf/float(max(hf)),thick=th,color=colordex('P'),psym=10
oplot,xnh,hn/float(max(hn)),thick=th,color=colordex('B'),psym=10
legend,['FUV: '+strtrim(long(total(hf)),2),'NUV: '+strtrim(long(total(hn)),2)],$
	thick=[th,th],color=[colordex('P'),colordex('B')], linesty=[0,0], $
	charsi=si*2.0,charthi=th,/right,box=0
;
if keyword_set(list) then begin
	printf,ol,'# '+tlab
	free_lun,ol
endif
;
q=''
read,'next: ',q
;
; plot Fvar vs Nobs
!p.multi=[0,1,2]
plot,nnn(ng),nfv(ng),thick=th,charsi=si, charthi=th, xthick=th, ythick=th, $
	xcharsi=si, ycharsi=si, title=tlab, /nodata, $
	xtitle='N OBS', xran=[1,3000],xsty=1,/xlog, $
	ytitle='Fvar', yran=[-0.2,4.],ysty=1
oplot,nnn(ng),nfv(ng),psym=4,thick=th,color=colordex('B')
plot,nnn(ng),nfv(ng),thick=th,charsi=si, charthi=th, xthick=th, ythick=th, $
	xcharsi=si, ycharsi=si, /nodata, $
	xtitle='N OBS', xran=[1,3000],xsty=1,/xlog, $
	ytitle='Fvar', yran=[-0.2,4.],ysty=1
oplot,nnf(fg),ffv(fg),psym=5,thick=th,color=colordex('P')
;
!p.multi=0
;
return
end
