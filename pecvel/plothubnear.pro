pro plothubnear,stats,wom=wom,omol=omol,scale=scale,indiv=indiv,test=test,$
		notitle=notitle,raw=raw
;
; get model number
cd,'./',cur=cwd
sta=strsplit(cwd,'/',/extract)
mdstr=sta(n_elements(sta)-1)
mdl=fix(strmid(mdstr,3,2))
;
; get fit parameters and type of correction
pars='wom'
type='indiv'
;
if keyword_set(omol) then pars='omol'
if keyword_set(wom) then pars='wom'
if keyword_set(scale) then type='scale'
if keyword_set(indiv) then type='indiv'
;
; read cosmo params
if keyword_set(raw) then $
	ifile = '../'+pars+'_raw_values_'+type+'.txt' $
else	ifile = '../'+pars+'_values_'+type+'.txt'
;
readcol,ifile,mods,dh1,dhe,p1,p1p,p1m,p2,p2p,p2m,form='i,f,f,f,f,f,f,f,f',/sile
;
; find which model we're examining
t=where(mods eq mdl, n)
if n eq 1 then begin
	t = t(0)
	dh1 = dh1(t)
	dhe = dhe(t)
endif else begin
	print,'Error - params not found for model number: ',mdl
	return
endelse
;
; get appropriate cosmo params
if pars eq 'omol' then begin
	nom = p1(0)	; uncorrected parameters
	nomp = p1p(0)
	nomm = p1m(0)
	nol = p2(0)
	nolp = p2p(0)
	nolm = p2m(0)
	nw = -1.d0
	nwp = 0.
	nwm = 0.
	com = p1(t)	; corrected parameters
	comp = p1p(t)
	comm = p1m(t)
	col = p2(t)
	colp = p2p(t)
	colm = p2m(t)
	cw = -1.d0
	cwp = 0.
	cwm = 0.
endif else begin
	nw = p2(0)	; uncorrected parameters
	nwp = p2p(0)
	nwm = p2m(0)
	nom = p1(0)
	nomp = p1p(0)
	nomm = p1m(0)
	nol = 1.d0 - nom
	nolp = 0.
	nolm = 0.
	cw = p2(t)	; corrected parameters
	cwp = p2p(t)
	cwm = p2m(t)
	com = p1(t)
	comp = p1p(t)
	comm = p1m(t)
	col = 1.d0 - com
	colp = 0.
	colm = 0.
endelse
;
; get nuisance params (a,b,scrm)
;
; uncorrected
get_parsum,'../mod00/'+pars+'_parsum.txt',npnam,nptyp,npval
t=where(strpos(npnam,'alpha') ge 0)
nalpha=npval(2,t(0))
t=where(strpos(npnam,'beta') ge 0)
nbeta=npval(2,t(0))
t=where(strpos(npnam,'mathcal{M}') ge 0)
nscrm=npval(0,t(0))
;
; corrected
get_parsum,mdstr+'_'+pars+'_parsum_'+type+'.txt',cpnam,cptyp,cpval
t=where(strpos(cpnam,'alpha') ge 0)
calpha=cpval(2,t(0))
t=where(strpos(cpnam,'beta') ge 0)
cbeta=cpval(2,t(0))
t=where(strpos(cpnam,'mathcal{M}') ge 0)
cscrm=cpval(0,t(0))
;
; read in uncorrected sn data
readsne,'../mod00/Astier_sne.dat',names,data
;
nz = reform(data(0,*))
nmi = reform(data(3,*))
mie = reform(data(4,*))
si = reform(data(5,*))
sie = reform(data(6,*))
ci = reform(data(7,*))
cie = reform(data(6,*))
;
; read in corrected sn data
readsne,mdstr+'_sne_'+type+'.dat',cames,cata
;
cz = reform(cata(0,*))
cmi = reform(cata(3,*))
;
; for plotting
pz = (findgen(1000)+1.)/5000.0
pvel = nz-cz
;
; compute m_theory
;
; constants
c = 2.99792458d5                ;;  speed of light in km/s
h0 = 70.d0			;;  Hubble consant in km/s/Mpc
;
; sne calibrated abs magnitude
nsnm = nscrm + 5.d0 * alog10( h0/100.d0 ) - 42.38410d0
csnm = cscrm + 5.d0 * alog10( h0/100.d0 ) - 42.38410d0
;
; start with D_L
if pars eq 'omol' then begin
	ndl = sullivanlumdist(nz,omega_l=nol,omega_m=nom)
	pndl = sullivanlumdist(pz,omega_l=nol,omega_m=nom)
	cdl = sullivanlumdist(cz,omega_l=col,omega_m=com)
	pcdl = sullivanlumdist(pz,omega_l=col,omega_m=com)
endif else begin
	ndl = sullivanlumdist(nz,w=nw,omega_m=nom)
	pndl = sullivanlumdist(pz,w=nw,omega_m=nom)
	cdl = sullivanlumdist(cz,w=cw,omega_m=com)
	pcdl = sullivanlumdist(pz,w=cw,omega_m=com)
endelse
;
; m_theory
nmth = 5.d0 * alog10 ( ndl * h0/c ) - nalpha * (si-1.d0) + nbeta * ci + nscrm
cmth = 5.d0 * alog10 ( cdl * h0/c ) - calpha * (si-1.d0) + cbeta * ci + cscrm
;
; read fit errors from extout file
; no_scale version
efile = '../mod00/Astier_'+pars+'_extout.txt'
readcol,efile,nsn,v1,v2,v3,v4,v5,v6,v7,nerrin,form='a,f,f,f,f,f,f,f,f',/silent
; correlate errors by name
nmube = nz-nz
for i=0,n_elements(nz)-1 do begin
	pp=strpos(nsn,names(i))
	t=where(pp ge 0, n)
	if n eq 1 then $
		nmube(i) = nerrin(t(0)) $
	else	print,'No error for: ',names(i)
endfor
;
; corrected version
efile = mdstr+'_'+pars+'_extout_'+type+'.txt'
readcol,efile,csn,v1,v2,v3,v4,v5,v6,v7,cerrin,form='a,f,f,f,f,f,f,f,f',/silent
; correlate errors by name
cmube = cz-cz
for i=0,n_elements(cz)-1 do begin
	pp=strpos(csn,cames(i))
	t=where(pp ge 0, n)
	if n eq 1 then $
		cmube(i) = cerrin(t(0)) $
	else	print,'No error for: ',cames(i)
endfor
;
; chi-2
anchi2 = total( (nmi - nmth)^2 / nmube^2 )
acchi2 = total( (cmi - cmth)^2 / cmube^2 )
print,'ALL Uncorrected chi^2: ',anchi2
print,'ALL   Corrected chi^2: ',acchi2
lowz = where(cz lt 0.15)
nchi2 = total( (nmi(lowz) - nmth(lowz))^2 / nmube(lowz)^2 )
cchi2 = total( (cmi(lowz) - cmth(lowz))^2 / cmube(lowz)^2 )
print,'LOZ Uncorrected chi^2: ',nchi2
print,'LOZ   Corrected chi^2: ',cchi2
;
; mub's
nmub = nmi + nalpha * (si-1.d0) - nbeta * ci - nsnm
cmub = cmi + calpha * (si-1.d0) - cbeta * ci - csnm
nmubth = 5.d0 * alog10( ndl * 1.d5 )
cmubth = 5.d0 * alog10( cdl * 1.d5 )
pnmub = 5.d0 * alog10( pndl * 1.d5 )
pcmub = 5.d0 * alog10( pcdl * 1.d5 )
;
; residuals
nrsd = nmub - nmubth
crsd = cmub - cmubth
;
; test
if keyword_set(test) then begin
	nchirsd = (nmi - nmth)^2/nmube^2
	cchirsd = (cmi - cmth)^2/cmube^2
	!p.multi=0
	plot,nchirsd(lowz),cchirsd(lowz),psym=5,xsty=1,ysty=1,title=mdstr, $
		xtitle='uncor resid',ytitle='corr resid'
	oplot,[-100,100],[-100,100],linesty=2
	q=''
	read,'Next: ',q
endif
;
;!p.multi=[0,1,2]
a=[findgen(16)*(!pi*2/16.),0.]
;
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
!p.font=0
;
; get model number from directory name
cd,'./',current=cwd
sta=strsplit(cwd,'/',/extract)
dir=sta(n_elements(sta)-1)
;
pindex=intarr(n_elements(pvel))
indr=where(pvel ge 0., nr)
if nr gt 0 then pindex(indr) = colordex('red')
indb=where(pvel lt 0., nb)
if nb gt 0 then pindex(indb) = colordex('blue')
;
; get model params
flist=file_search('neill*clusvp1',count=nf)
if nf eq 1 then $
	tlab = dir+': '+pars+', '+type+', '+strmid(flist(0),6) $
else	tlab = dir+': '+pars+', '+type
if keyword_set(notitle) then tlab=''
plot,nz,nmub,psym=8,ytitle='!Mm!3!DB!N',yran=[32.7,39.3],ysty=1,$
  xran=[0.007,0.128],xsty=1,charsi=1.5,charthi=3,xthi=3,ythi=3,/nodata, $
  title=tlab,position=[0.12,0.45,0.96,0.92],xtickname=replicate(' ',6)
errplot,cz,cmub-cmube,cmub+cmube,thick=3,width=0.
oplot,pz,pnmub,thick=3,color=colordex('orange')
oplot,pz,pcmub,thick=3,color=colordex('green'),linesty=2
;
usersym,cos(a)*.75,sin(a)*.75
oplot,nz,nmub,psym=8 ;,color=colordex('blue')
usersym,cos(a)*.75,sin(a)*.75,/fill
oplot,cz,cmub,psym=8 ;,color=colordex('red')
for i=0,n_elements(nz)-1 do $
	oplot,[nz(i),cz(i)],[nmub(i),cmub(i)],color=pindex(i),thick=3
;
legend,[textoidl('z_{OBS}'),textoidl('z_{COR}')],psym=[8,8], $
	box=0,charsi=1.2,charthi=3,spac=2.5,fill=[0,1], $
	usersym=transpose([[cos(a)*.75],[sin(a)*.75]])
if pars eq 'omol' then begin
	lab1 =textoidl('\Omega_{m,OBS} = '+string(nom,form='(f5.3)')+$
       '^{+'+string(nomp,form='(f5.3)')+'}_{-'+string(nomm,form='(f5.3)')+'},'+$
	        ' \Omega_{\Lambda,OBS} = '+string(nol,form='(f5.3)')+$
       '^{+'+string(nolp,form='(f5.3)')+'}_{-'+string(nolm,form='(f5.3)')+'}')
	lab2 =textoidl('\Omega_{m,COR} = '+string(com,form='(f5.3)')+$
       '^{+'+string(comp,form='(f5.3)')+'}_{-'+string(comm,form='(f5.3)')+'},'+$
	 	' \Omega_{\Lambda,COR} = '+string(col,form='(f5.3)')+$
       '^{+'+string(colp,form='(f5.3)')+'}_{-'+string(colm,form='(f5.3)')+'}')
       clab = 'w = -1'
endif else begin
	lab1 =textoidl('\Omega_{m,OBS} = '+string(nom,form='(f5.3)')+$
       '^{+'+string(nomp,form='(f5.3)')+'}_{-'+string(nomm,form='(f5.3)')+'},'+$
	                     ' w_{OBS} = '+string(nw,form='(f6.3)')+$
       '^{+'+string(nwp,form='(f5.3)')+'}_{-'+string(nwm,form='(f5.3)')+'}')
	lab2 =textoidl('\Omega_{m,COR} = '+string(com,form='(f5.3)')+$
       '^{+'+string(comp,form='(f5.3)')+'}_{-'+string(comm,form='(f5.3)')+'},'+$
	                     ' w_{COR} = '+string(cw,form='(f6.3)')+$
       '^{+'+string(cwp,form='(f5.3)')+'}_{-'+string(cwm,form='(f5.3)')+'}')
       clab = '\Omega = 1'
endelse
if keyword_set(raw) then $
	plab = 'SNLS' $
else	plab = 'SNLS + BAO prior'
legend,[textoidl(clab),plab],charsi=1.2,charthi=3,spac=2.9,$
	pos=[0.127,38], box=0,/right
legend,[lab1,lab2], linesty=[0,2], $
	color=[colordex('orange'),colordex('green')],thick=3,$
	box=0,charsi=1.2,charthi=3,spac=2.9,pos=[0.055,36.3],pspac=1.25
;
plot,nz,nrsd,psym=8,xtitle='z',ytitle='!MD m!3!DB!N',yran=[-0.7,.99],ysty=1,$
  xran=[0.007,0.128],xsty=1,charsi=1.5,charthi=3,xthi=3,ythi=3,/nodata,$
  position=[0.12,0.10,0.96,0.45],/noerase
errplot,cz,crsd-cmube,crsd+cmube,thick=3,width=0
oplot,[-100,100],[0,0];,linesty=2
;
usersym,cos(a)*.75,sin(a)*0.75
oplot,nz,nrsd,psym=8 ;,color=colordex('blue')
usersym,cos(a)*.75,sin(a)*0.75,/fill
oplot,cz,crsd,psym=8 ;,color=colordex('red')
mean=wmean(nrsd(lowz),nmube(lowz))
stdv=wstdev(nrsd(lowz),nmube(lowz))
scat=stddev(nrsd(lowz))
mean2=wmean(crsd(lowz),cmube(lowz))
scat2=stddev(crsd(lowz))
print,'Uncorrected offset: ',mean,' +- ',stdv,' scatter: ',scat
print,'Corrected offset  : ',mean2,' +- ',stdv,' scatter: ',scat2
oplot,[0,100],[mean,mean],color=colordex('orange'),thick=3
oplot,[0,100],[mean2,mean2],color=colordex('green'),linesty=2,thick=3
for i=0,n_elements(nz)-1 do $
	oplot,[nz(i),cz(i)],[nrsd(i),crsd(i)],color=pindex(i),thick=3
legend,[textoidl('<\Delta \mu_{B,OBS}> = ')+strn(mean,form='(f6.3)')+' '+ $
	textoidl('\pm')+' '+strn(stdv,form='(f5.3)')+', RMS = '+$
		      strn(scat,form='(f5.3)'), $
	textoidl('<\Delta \mu_{B,COR}> = ')+strn(mean2,form='(f6.3)')+' '+ $
	textoidl('\pm')+' '+strn(stdv,form='(f5.3)')+', RMS = '+$
		      strn(scat2,form='(f5.3)')], $
	box=0,charsi=1.2,charthi=3,spac=2.5,pos=[0.055,0.88], pspac=1.25, $
	color=[colordex('orange'),colordex('green')],linesty=[0,2],thick=3
legend,[textoidl('\chi^2_{OBS,ALL} = ')+strn(anchi2,form='(f6.1)')+ $
  ',  '+textoidl('\chi^2_{OBS,z<0.15} = ')+strn(nchi2,form='(f6.2)'), $
	textoidl('\chi^2_{COR,ALL} = ')+strn(acchi2,form='(f6.1)')+ $
  ',  '+textoidl('\chi^2_{COR,z<0.15} = ')+strn(cchi2,form='(f6.2)')], $
	box=0,charsi=1.2,charthi=3,spac=2.5,/right,pos=[0.127,0]
;
;!p.multi=0
!p.font=-1
;
stats = [ mean,stdv,scat,anchi2,nchi2,mean2,stdv,scat2,acchi2,cchi2 ]
return
end
