pro peg_compare,samnam,libnam,verbose=verbose,examine=examine, $
	use_median_values=use_median_values,ps=ps
;+
;	peg_compare - compare the lephare SED fits using the PEGASE.2
;		library with the ZPEG fits
;
; INPUTS:
;	samnam - sample id (string) example: 'S1'
;	libnam - LePhare library name (string) example: 'SNLSPEG'
;
; KEYWORDS:
;	verbose - extra output
;	examine - display outlier fit plots
;	ps      - postscript output
;
; NOTES:
;	Assumes that zpeg results are in file 'zpeg_results.sav' in current dir
;	Assumes that lephare results are in file 'sedfit_lephare'+samnam+'.out'
;
; HISTORY:
;	17-NOV-2010 - jdn, written
;-
; check inputs
if n_params(0) lt 2 then begin
	print,'PEG_COMPARE: Usage - peg_compare,sample_id,library_name [,/verbose,/examine]'
	return
endif
;
; zpeg results
svfil='zpeg_results.sav'
;
restore,svfil
;
; pull out pegase data
sfrlm = [-3.,8.]
ssfrlm= [-13.,-3]
get_peg,data,agel,agel_m,agel_p,mstar,mstar_m,mstar_p,sfr,sfr_m,sfr_p, $
	ssfr,ssfr_m,ssfr_p,ebv,ch2,name,bnd,count=nsn, $
	sfrl=sfrlm,ssfrl=ssfrlm,/range_errors
mstar_m = mstar_m - mstar
mstar_p = mstar - mstar_p
agel_m  = agel_m - agel
agel_p  = agel - agel_p
nparams = 2	; at least two free params: scaling, age, z is fixed
ch2 = ch2/float(bnd-(nparams+1))
agel = agel / 1.e9 & agel_m = agel_m / 1.e9 & agel_p = agel_p / 1.e9	; Gyr
nseq  = intarr(nsn)
nname = strarr(nsn)
nagel = fltarr(nsn)
nagel_p = nagel
nagel_m = nagel
nmstar = fltarr(nsn)
nmstar_p = nmstar
nmstar_m = nmstar
nsfr = fltarr(nsn) -999.
nsfr_p = nsfr
nsfr_m = nsfr
nssfr = fltarr(nsn) -999.
nssfr_p = nssfr
nssfr_m = nssfr
nch2 = fltarr(nsn)
nnbnd = intarr(nsn)
;
; get lephare results
readcol,'sedfit_lephare'+samnam+'.out',seq,chib,ebv,nbnd,nbndu, $
	nageb,nag_m,nag,nag_p,nmstarb,nms_m,nms,nms_p,nsfrb,nsf_m,nsf,nsf_p, $
	nssfrb,nss_m,nss,nss_p,inname, comment='#', skip=61, $
	format='i,f,f,i,i,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,f,a'
nlp = n_elements(seq)
nmtch = 0L
nused = intarr(nlp)
use_med = keyword_set(use_median_values)
ubest = intarr(nsn)
umed  = intarr(nsn)
print,'Matching ZPEG to LePhare...'
print,'Missing from LePhare: '
for i=0,nsn-1 do begin
	t=where(strcmp(inname,name[i]) eq 1, nt)
	if nt eq 1 then begin
		t=t[0]
		nused[t] = 1
		if keyword_set(verbose) then $
			print,i,name[i],inname[t],form='(i3,2x,2a-10)'
		nname[i] = inname[t]
		nseq[i] = seq[t]
		if nageb[t] gt 0. then ubest[i] = 1
		if nag[t] gt 0. then umed[i] = 1
		nagel[i] = (use_med) ? nag[t] : nageb[t]
		nagel_m[i] = nag_m[t] - nagel[i]
		nagel_p[i] = nagel[i] - nag_p[t]
		nmstar[i] = (use_med) ? nms[t] : nmstarb[t]
		nmstar_m[i] = nms_m[t] - nmstar[i]
		nmstar_p[i] = nmstar[i] - nms_p[t]
		ubest[i] = (nsfrb[t] gt -90.) ? 1 : 0
		umed[i] = (nsf[t] gt -90.) ? 1 : 0
		nsfr[i] = (use_med) ? nsf[t] : nsfrb[t]
		nsfr[i] = nsfr[i] > sfrlm[0]
		nsfr_m[i] = nsf_m[i] > sfrlm[0]
		nsfr_p[i] = nsf_p[i] > sfrlm[0]
		nssfr[i] = (use_med) ? nss[t] : nssfrb[t]
		nssfr[i] = nssfr[i] > ssfrlm[0]
		nssfr_m[i] = nss_m[t]
		nssfr_p[i] = nss_p[t]
		nnbnd[i] = nbnd[t]
		nch2[i] = chib[t]/float(nbnd[t]-(nparams+1))
		nmtch = nmtch + 1L
	endif else $
		print,name[i]
endfor
nagel = nagel / 1.e9 & nagel_m = nagel_m / 1.e9 & nagel_p = nagel_p / 1.e9 ; Gyr
;
t=where(nused eq 0, nt)
if nt gt 0 then begin
	print,'LePhare fits not used...'
	print,'  #   LpID  SN'
	for i=0,nt-1 do print,i+1,t[i]+1L,inname[t[i]],form='(i4,i6,2x,a)'
endif
exfmt='(i5,2x,a-10,4f9.3,i5,f9.3,i5)'
;stop
;
; set up plot
q=''
if use_med then begin
	psfr = 'peg_comp_med'
	leplab = ' LEPHARE (MED)'
endif else begin
	psfr = 'peg_comp_best'
	leplab = ' LEPHARE (BEST)'
endelse
font_store=!p.font
if keyword_set(ps) then begin
	psfile,psfr
	th=7
	si=1.5
	!p.font=0
	ylab='M* log!D10!N(M'+sunsymbol()+' )'
endif else begin
	th=2
	si=1.7
	ylab='M* log!D10!N(M!D!9n!3!N)'
endelse
deepcolor
!p.background=colordex('white')
!p.color=colordex('black')
;
; M*
xlab = ylab + ' ZPEG'
ylab = ylab + leplab
xrng = [7.5,12.5]
yrng = xrng
if use_med then $
	g = where(umed eq 1, nfn) $
else	g = where(ubest eq 1, nfn)
tlab = libnam+' LIB  N!DSN!N (Z/L/M/G) = '+ $
	strn(nsn)+'/'+strn(nlp)+'/'+strn(nmtch)+'/'+strn(nfn)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=xlab,xran=xrng,xsty=1, $
	ytitle=ylab,yran=yrng,ysty=1, title=tlab
;
vsym,24,/fill
if use_med then begin
	oploterror,mstar(g),nmstar(g),mstar_m(g),nmstar_m(g),/lobar,psym=8
	oploterror,mstar(g),nmstar(g),mstar_p(g),nmstar_p(g),/hibar,psym=8
endif else $
	oplot,mstar[g],nmstar[g],psym=8
oplot,[-1000,1000],[-1000,1000],linesty=5,thick=th
diff = nmstar[g] - mstar[g]
res=moment(diff)
ims,diff,dmean,dsig
print,'lgM* <lp - zp> = ',res[0],' +- ',sqrt(res[1]),' : 3sg = ',dmean,$
	' +- ',dsig, format='(a,f6.3,a,f6.3,a,f6.3,a,f6.3)'
grange = [res[0] - 3.0 * sqrt(res[1]), res[0] + 3.0 * sqrt(res[1])]
drange = [dmean - 3.0 * dsig, dmean + 3.0 * dsig]
if keyword_set(examine) then $
	print,'    #  SN            zplM*    lplM*    delta    zpch2 nbnd    lpch2 nbnd'
nrej = 0L
ndrej = 0L
for i=0,nfn-1 do begin
	p=g[i]
	diff = nmstar[p] - mstar[p]
	if diff lt grange[0] or diff gt grange[1] then begin
		nrej = nrej + 1L
		plots,mstar[p],nmstar[p],psym=5,symsi=2.
	endif
	if diff lt drange[0] or diff gt drange[1] then begin
		ndrej = ndrej + 1L
		plots,mstar[p],nmstar[p],psym=4,symsi=2.
		if keyword_set(examine) then begin
			print,nseq[p],name[p],mstar[p],nmstar[p],diff, $
				ch2[p],bnd[p],nch2[p],nnbnd[p], format=exfmt
			gfil = 'Id'+string(nseq[p],form='(i09)')+'.spec.gif'
			if file_test(gfil) then $
				spawn,'open '+gfil
		endif
	endif
endfor
legend,[textoidl('N>3\sigma = ')+strn(nrej) + ', <LP-ZP> = '+ $
	strtrim(string(res[0],form='(f9.3)'),2)+' +- '+ $
	strtrim(string(sqrt(res[1]),form='(f9.3)'),2), $
	textoidl('N>3\sigma = ')+strn(ndrej) + $
	textoidl(', <LP-ZP>_{3\sigma} = ')+ $
	strtrim(string(dmean,form='(f9.3)'),2)+' +- '+ $
	strtrim(string(dsig,form='(f9.3)'),2)],charsi=si,charthi=th,$
	psym=[5,4],box=0
;
if keyword_set(ps) then begin
	ylab = textoidl('<AGE>_L Gyr')
endif else begin
	ylab = '<AGE>!DL!N Gyr'
	read,'<next> ',q
endelse
;
; Age
xlab = ylab + ' ZPEG'
ylab = ylab + leplab
xrng = [-0.3,14.3]
yrng = [-0.3,15.1]
tlab = libnam+' LIB  N!DSN!N (Z/L/M/G) = '+ $
	strn(nsn)+'/'+strn(nlp)+'/'+strn(nmtch)+'/'+strn(nfn)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=xlab,xran=xrng,xsty=1, $
	ytitle=ylab,yran=yrng,ysty=1, title=tlab
;
vsym,24,/fill
if use_med then begin
	oploterror,agel(g),nagel(g),agel_m(g),nagel_m(g),/lobar,psym=8
	oploterror,agel(g),nagel(g),agel_p(g),nagel_p(g),/hibar,psym=8
endif else $
	oplot,agel[g],nagel[g],psym=8
oplot,[-1.,20.],[-1.,20.],linesty=5,thick=th
diff = nagel[g] - agel[g]
res=moment(diff)
ims,diff,dmean,dsig
print,'Age  <lp - zp> = ',res[0],' +- ',sqrt(res[1]),' : 3sg = ',dmean,$
	' +- ',dsig, format='(a,f6.3,a,f6.3,a,f6.3,a,f6.3)'
grange = [res[0] - 3.0 * sqrt(res[1]), res[0] + 3.0 * sqrt(res[1])]
drange = [dmean - 3.0 * dsig, dmean + 3.0 * dsig]
if keyword_set(examine) then $
	print,'    #  SN            zpAge    lpAge    delta    zpch2 nbnd    lpch2 nbnd'
nrej = 0L
ndrej = 0L
for i=0,nfn-1 do begin
	p=g[i]
	diff = nagel[p] - agel[p]
	if diff lt grange[0] or diff gt grange[1] then begin
		nrej = nrej + 1L
		plots,agel[p],nagel[p],psym=5,symsi=2.
	endif
	if diff lt drange[0] or diff gt drange[1] then begin
		ndrej = ndrej + 1L
		plots,agel[p],nagel[p],psym=4,symsi=2.
		if keyword_set(examine) then begin
			print,nseq[p],name[p],agel[p],nagel[p],diff, $
				ch2[p],bnd[p],nch2[p],nnbnd[p], format=exfmt
			gfil = 'Id'+string(nseq[p],form='(i09)')+'.spec.gif'
			if file_test(gfil) then $
				spawn,'open '+gfil
		endif
	endif
endfor
legend,[textoidl('N>3\sigma = ')+strn(nrej) + ', <LP-ZP> = '+ $
	strtrim(string(res[0],form='(f9.3)'),2)+' +- '+ $
	strtrim(string(sqrt(res[1]),form='(f9.3)'),2), $
	textoidl('N>3\sigma = ')+strn(ndrej) + $
	textoidl(', <LP-ZP>_{3\sigma} = ')+ $
	strtrim(string(dmean,form='(f9.3)'),2)+' +- '+ $
	strtrim(string(dsig,form='(f9.3)'),2)],charsi=si,charthi=th,$
	psym=[5,4],box=0
;
if keyword_set(ps) then begin
	ylab = 'SFR log!D10!N(M'+sunsymbol()+'/yr )'
endif else begin
	ylab = 'SFR log!D10!N(M!D!9n!3!N/yr)'
	read,'<next> ',q
endelse
;
; SFR
xlab = ylab + ' ZPEG'
ylab = ylab + leplab
xrng = [-3.3,3.0]
yrng = [-3.8,8.0]
if use_med then begin
	g=where(umed eq 1,nfn)
	b=where(umed eq 1 and sfr gt sfrlm[0] and nsfr gt sfrlm[0],nbs)
	u0=where(umed eq 1 and sfr eq sfrlm[0],nu0)
	u1=where(umed eq 1 and nsfr eq sfrlm[0],nu1)
endif else begin
	g=where(ubest eq 1,nfn)
	u0=where(ubest eq 1 and sfr eq sfrlm[0],nu0)
	u1=where(ubest eq 1 and nsfr eq sfrlm[0],nu1)
endelse
tlab = libnam+' LIB  N!DSN!N (Z/L/M/G) = '+ $
	strn(nsn)+'/'+strn(nlp)+'/'+strn(nmtch)+'/'+strn(nfn)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=xlab,xran=xrng,xsty=1, $
	ytitle=ylab,yran=yrng,ysty=1, title=tlab
;
vsym,24,/fill
oplot,sfr[g],nsfr[g],psym=8
if use_med then begin
    oploterror,sfr(b),nsfr(b),sfr_m(b)-sfr[b],nsfr_m(b)-nsfr[b],/lobar,psym=8
    oploterror,sfr(b),nsfr(b),sfr[b]-sfr_p(b),nsfr[b]-nsfr_p(b),/hibar,psym=8
    if nu0 gt 0 then $
    	olimplot,sfr[u0],nsfr[u0],/lower,/xlimit
    if nu1 gt 0 then $
	olimplot,sfr[u1],nsfr[u1],/lower
endif else begin
    if nu0 gt 0 then $
    	olimplot,sfr[u0],nsfr[u0],/lower,/xlimit
    if nu1 gt 0 then $
	olimplot,sfr[u1],nsfr[u1],/lower
endelse
oplot,[-1000,1000],[-1000,1000],linesty=5,thick=th
zpg = sfr[g]
lph = nsfr[g]
p = where(zpg ge sfrlm[0] and lph ge sfrlm[0])
diff = lph[p] - zpg[p]
res=moment(diff)
ims,diff,dmean,dsig
print,'SFR  <lp - zp> = ',res[0],' +- ',sqrt(res[1]),' : 3sg = ',dmean,$
	' +- ',dsig, format='(a,f6.3,a,f6.3,a,f6.3,a,f6.3)'
grange = [res[0] - 3.0 * sqrt(res[1]), res[0] + 3.0 * sqrt(res[1])]
drange = [dmean - 3.0 * dsig, dmean + 3.0 * dsig]
if keyword_set(examine) then $
	print,'    #  SN            zpSFR    lpSFR    delta    zpch2 nbnd    lpch2 nbnd'
nrej = 0L
ndrej = 0L
for i=0,nfn-1 do begin
	p=g[i]
	diff = nsfr[p] - sfr[p]
	if diff lt grange[0] or diff gt grange[1] then begin
		nrej = nrej + 1L
		plots,sfr[p],nsfr[p],psym=5,symsi=2.
	endif
	if diff lt drange[0] or diff gt drange[1] then begin
		ndrej = ndrej + 1L
		plots,sfr[p],nsfr[p],psym=4,symsi=2.
		if keyword_set(examine) then begin
			print,nseq[p],name[p],sfr[p],nsfr[p],diff, $
				ch2[p],bnd[p],nch2[p],nnbnd[p], format=exfmt
			gfil = 'Id'+string(nseq[p],form='(i09)')+'.spec.gif'
			if file_test(gfil) then $
				spawn,'open '+gfil
		endif
	endif
endfor
legend,[textoidl('N>3\sigma = ')+strn(nrej) + ', <LP-ZP> = '+ $
	strtrim(string(res[0],form='(f9.3)'),2)+' +- '+ $
	strtrim(string(sqrt(res[1]),form='(f9.3)'),2), $
	textoidl('N>3\sigma = ')+strn(ndrej) + $
	textoidl(', <LP-ZP>_{3\sigma} = ')+ $
	strtrim(string(dmean,form='(f9.3)'),2)+' +- '+ $
	strtrim(string(dsig,form='(f9.3)'),2)],charsi=si,charthi=th,$
	psym=[5,4],box=0
;
if keyword_set(ps) then begin
	ylab = textoidl('SFR/M* log_{10}(yr^{-1})')
endif else begin
	ylab = 'SFR/M* log!D10!N(yr!U-1!N)'
	read,'<next> ',q
endelse
;
; SSFR
xlab = ylab + ' ZPEG'
ylab = ylab + leplab
xrng = [-13.3,-7.6]
yrng = [-13.3,-7.6]
if use_med then begin
	b=where(umed eq 1 and ssfr gt ssfrlm[0] and nssfr gt ssfrlm[0],nbs)
	u0=where(umed eq 1 and ssfr eq ssfrlm[0],nu0)
	u1=where(umed eq 1 and nssfr eq ssfrlm[0],nu1)
endif else begin
	u0=where(ubest eq 1 and ssfr eq ssfrlm[0],nu0)
	u1=where(ubest eq 1 and nssfr eq ssfrlm[0],nu1)
endelse
tlab = libnam+' LIB  N!DSN!N (Z/L/M/G) = '+ $
	strn(nsn)+'/'+strn(nlp)+'/'+strn(nmtch)+'/'+strn(nfn)
plot,[0,0],[1,1],/nodata,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=xlab,xran=xrng,xsty=1, $
	ytitle=ylab,yran=yrng,ysty=1, title=tlab
;
vsym,24,/fill
oplot,ssfr[g],nssfr[g],psym=8
if use_med then begin
    oploterror,ssfr(b),nssfr(b),ssfr_m(b)-ssfr[b],nssfr_m(b)-nssfr[b],/lobar,psym=8
    oploterror,ssfr(b),nssfr(b),ssfr[b]-ssfr_p(b),nssfr[b]-nssfr_p(b),/hibar,psym=8
    if nu0 gt 0 then $
    	olimplot,ssfr[u0],nssfr[u0],/lower,/xlimit
    if nu1 gt 0 then $
	olimplot,ssfr[u1],nssfr[u1],/lower
endif else begin
    if nu0 gt 0 then $
    	olimplot,ssfr[u0],nssfr[u0],/lower,/xlimit
    if nu1 gt 0 then $
	olimplot,ssfr[u1],nssfr[u1],/lower
endelse
oplot,[-1000,1000],[-1000,1000],linesty=5,thick=th
diff = nssfr[g] - ssfr[g]
res=moment(diff)
ims,diff,dmean,dsig
print,'sSFR <lp - zp> = ',res[0],' +- ',sqrt(res[1]),' : 3sg = ',dmean,$
	' +- ',dsig, format='(a,f6.3,a,f6.3,a,f6.3,a,f6.3)'
grange = [res[0] - 3.0 * sqrt(res[1]), res[0] + 3.0 * sqrt(res[1])]
drange = [dmean - 3.0 * dsig, dmean + 3.0 * dsig]
if keyword_set(examine) then $
	print,'    #  SN           zpsSFR   lpsSFR    delta    zpch2 nbnd    lpch2 nbnd'
nrej = 0L
ndrej = 0L
for i=0,nfn-1 do begin
	p=g[i]
	diff = nssfr[p] - ssfr[p]
	if diff lt grange[0] or diff gt grange[1] then begin
		nrej = nrej + 1L
		plots,ssfr[p],nssfr[p],psym=5,symsi=2.
	endif
	if diff lt drange[0] or diff gt drange[1] then begin
		ndrej = ndrej + 1L
		plots,ssfr[p],nssfr[p],psym=4,symsi=2.
		if keyword_set(examine) then begin
			print,nseq[p],name[p],ssfr[p],nssfr[p],diff, $
				ch2[p],bnd[p],nch2[p],nnbnd[p], format=exfmt
			gfil = 'Id'+string(nseq[p],form='(i09)')+'.spec.gif'
			if file_test(gfil) then $
				spawn,'open '+gfil
		endif
	endif
endfor
legend,[textoidl('N>3\sigma = ')+strn(nrej) + ', <LP-ZP> = '+ $
	strtrim(string(res[0],form='(f9.3)'),2)+' +- '+ $
	strtrim(string(sqrt(res[1]),form='(f9.3)'),2), $
	textoidl('N>3\sigma = ')+strn(ndrej) + $
	textoidl(', <LP-ZP>_{3\sigma} = ')+ $
	strtrim(string(dmean,form='(f9.3)'),2)+' +- '+ $
	strtrim(string(dsig,form='(f9.3)'),2)],charsi=si,charthi=th,$
	psym=[5,4],box=0
;
if keyword_set(ps) then begin
	ylab = textoidl('\chi^2_{\nu}')
endif else begin
	ylab = 'CHI!U2!N/NDF'
	read,'<next> ',q
endelse
;
; CHI2
xlab = ylab + ' ZPEG'
ylab = ylab + leplab
xrng = alog10(minmax(ch2[g]))
del = xrng[1] - xrng[0]
xrng[0] = xrng[0] - del * 0.3
xrng[1] = xrng[1] + del * 0.3
xrng = 10.^xrng
yrng = alog10(minmax(nch2[g]))
del = yrng[1] - yrng[0]
yrng[0] = yrng[0] - del * 0.3
yrng[1] = yrng[1] + del * 0.3
yrng = 10.^yrng
;g=where(ch2 gt 0. and nch2 gt 0., nfn)
tlab = libnam+' LIB  N!DSN!N (Z/L/M/G) = '+ $
	strn(nsn)+'/'+strn(nlp)+'/'+strn(nmtch)+'/'+strn(nfn)
plot,ch2[g],nch2[g],psym=4,xthick=th,ythick=th,thick=th,charsi=si,charthi=th, $
	xtitle=xlab,xran=xrng,xsty=1,/xlog, $
	ytitle=ylab,yran=yrng,ysty=1,/ylog, title=tlab
;
vsym,24,/fill
oplot,[1.e-19,1.e19],[1.e-19,1.e19],linesty=5,thick=th
;
if keyword_set(ps) then psclose
;
!p.font=font_store
;
print,'old/new: ',nsn,'/',nfn
;
return
end
