function qsofvar,ifile,hline=hline,nobs=nobs,avmg=avmg,sncut=sncut,expcut=expcut
;+
; qsofvar - calculate Fvar from the qso photometry
;-
fvar=[-1.,-1.]
hline=''
nobs=[0,0]
avmg=[-99.,-99.]
;
; check for NO COVERAGE
rec=''
openr,il,ifile,/get_lun
for j=0,1 do readf,il,rec
if strpos(rec,'NO') ge 0 then return,fvar
free_lun,il
;
rdqsophot,ifile,ra,dec,dis,fm,fme,nm,nme,obd,expt,hline=hline
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
if ngf gt 1 then begin
	fm = 10.0^(-0.4*(fm(gf)-18.82))	; flux
	fme = fme(gf) * fm / 1.0857
	wf = 1./fme^2
	fmav = total(fm*wf)/total(wf)
	del = abs(fm-fmav)
	wf = wf / ( 1. + (del/2.)^2 )
	fmav = total(fm*wf)/total(wf)
	s2 = total( (fm - fmav)^2 ) / float(ngf)
	d2 = total( fme^2 ) / float(ngf)
	fvar[0] = sqrt( (s2 - d2)>0. ) / fmav
	avmg[0] = -2.5*alog10(fmav) + 18.82
endif else fvar[0] = -1.
if ngn gt 1 then begin
	nm = 10.0^(-0.4*(nm(gn)-20.08))	; flux
	nme = nme(gn) * nm / 1.0857
	wn = 1./nme^2
	nmav = total(nm*wn)/total(wn)
	del = abs(nm-nmav)
	wn = wn / (1. + (del/2.)^2 )
	nmav = total(nm*wn)/total(wn)
	s2 = total( (nm - nmav)^2 ) / float(ngn)
	d2 = total( nme^2 ) / float(ngn)
	fvar[1] = sqrt( (s2 - d2)>0. ) / nmav
	avmg[1] = -2.5*alog10(nmav) + 20.08
endif else fvar[1] = -1.
nobs=[ngf,ngn]
;
return,fvar
end
