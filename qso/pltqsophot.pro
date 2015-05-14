pro pltqsophot,ifile,sncut=sncut,expcut=expcut,yrange=yrange
;+
; pltqsophot - plot the photometry for a given qso
;
;-
flist=file_search(ifile, count=nf)
if nf ne 1 then begin
	print,'PLTQSOPHOT: Error - file not found: ',ifile
	return
endif
;
; check for NO COVERAGE
rec=''
openr,il,ifile,/get_lun
for j=0,1 do readf,il,rec
free_lun,il
if strpos(rec,'NO') ge 0 then begin
	print,'PLTQSOPHOT: Error - no coverage for qso in: ',ifile
	return
endif
;
; read photometry
rdqsophot,ifile,ra,dec,dis,fm,fme,nm,nme,obdate,expt,hline=hdr
ntp=n_elements(ra)
;
for i=0,2 do zspc = gettok(hdr,' ')
for i=0,1 do loiii= gettok(hdr,' ')
;jd=dblarr(ntp)
;for i=0,ntp-1 do jd(i) = date_parse(obdate(i))
fm = reform(fm(*,3))
fme= reform(fme(*,3))	; aper 4
nm = reform(nm(*,3))
nme= reform(nme(*,3))
if keyword_set(sncut) then begin
	if sncut gt 0 then $
		snc = sncut $
	else	snc = 2.0
endif else	snc = 1.0	; no cut
if keyword_set(expcut) then begin
	if expcut gt 0 then $
		exc = expcut $
	else	exc = 200.	; AIS cut
endif else	exc = 0.	; no cut
mlim = 1.0857362/snc
gf=where(fm gt 0. and fme le mlim and fme ge 0. and expt gt exc, ngf)
gn=where(nm gt 0. and nme le mlim and nme ge 0. and expt gt exc, ngn)
zf=where(fm gt 0., nzf)
zn=where(nm gt 0., nzn)
;
cd,'./',current=cwd
sta=strsplit(cwd,'/',/extract)
dir=sta(n_elements(sta)-1)
tlab = dir + '   ' + ifile
if keyword_set(sncut) then $
	tlab = tlab + '   S/N cut: '+string(sncut,form='(f4.1)') $
else	tlab = tlab + '   NO S/N cut'
if keyword_set(expcut) then $
	tlab = tlab + '   EXPTIME cut: '+string(expcut,form='(f5.1)')+' s' $
else	tlab = tlab + '   NO EXPTIME cut'
;
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
th=3
si=1.5
;
xran=[-(ntp/20.),ntp+ntp/20.]
if keyword_set(yrange) then begin
	if n_elements(yrange) eq 2 then $
		yran=yrange $
	else	begin
		print,'PLTQSOPHOT: Error - invalid y-range: ',yrange
		return
	endelse
endif else $
if nzf gt 0 and nzn gt 0 then $
	yran=[max([fm(zf)+fme(zf),nm(zn)+nme(zn)]), $
	      min([fm(zf)-fme(zf),nm(zn)-nme(zn)])] $
else	if nzf le 0 and nzn gt 0 then $
	yran=[max([nm(zn)+nme(zn)]),min([nm(zn)-nme(zn)])] $
else	if nzn le 0 and nzf gt 0 then $
	yran=[max([fm(zf)+fme(zf)]),min([fm(zf)-fme(zf)])] $
else	begin
	print,'PLTQSOPHOT: Error - no good points'
	return
endelse
;
; get variability
if keyword_set(sncut) then $
	if keyword_set(expcut) then $
		fvar=qsofvar(ifile, hline=hdr,nobs=nobs,avmg=avmg, $
			sncut=sncut,expcut=expcut) $
	else	fvar=qsofvar(ifile, hline=hdr,nobs=nobs,avmg=avmg, $
			sncut=sncut) $
else 	if keyword_set(expcut) then $
		fvar=qsofvar(ifile,hline=hdr,nobs=nobs,avmg=avmg, $
			expcut=expcut) $
	else	fvar=qsofvar(ifile,hline=hdr,nobs=nobs,avmg=avmg)
;
note=string('Fav Nav z L(OIII): ',avmg,zspc,loiii,form='(a,2f8.3,1x,a,1x,a)')
;
plot,fm,thick=th,charsi=si, charthi=th, xthick=th, ythick=th, $
	xcharsi=si, ycharsi=si, title=tlab, psym=5, symsi=si, $
	xtitle='N OBS', xrange=xran, xsty=1, $
	ytitle='AB MAG', yrange=yran, ysty=1
oplot,nm,thick=th,psym=6,symsi=si
oploterror,fm,fme,psym=3
if ngf gt 0 then $
	oplot,gf,fm(gf),thick=th,color=colordex('P'),psym=5,symsi=si
oploterror,nm,nme,psym=3
if ngn gt 0 then $
	oplot,gn,nm(gn),thick=th,color=colordex('B'),psym=6,symsi=si
legend,['FUV: '+strtrim(ngf,2) + '  Fvar: ' + string(fvar[0],form='(f6.3)'), $
	'NUV: '+strtrim(ngn,2) + '  Fvar: ' + string(fvar[1],form='(f6.3)')],$
	thick=[th,th],color=[colordex('P'),colordex('B')], psym=[5,6], $
	charsi=si*2.0,charthi=th,/right,box=0
legend,[note, ' '],charsi=si*2.0,charthi=th,/bottom,box=0
;
return
end
