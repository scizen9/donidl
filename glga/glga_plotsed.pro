pro glga_plotsed,lfile,silent=silent,ps=ps,glgaout=glgaout
;+
; plot galaxy integrated SED
;
; lfile - list of objects with one line per object with these columns:
;       id
;       ra,dec  - degrees
;       majdiam,mindiam - arcmin
;       pa      - degrees
;	type	- galaxy type
;
; keywords:
;	ps - create postscript output
;	silent - supress output
;
;-
common filter_info
;
; photometry bands
bands = ['FUV','NUV','u','g','r','i','z','j','h','k','w1','w2','w3','w4']
shids = ['','','','','','','','J-2mass','H-2mass','Ks-2mass','','','','']
nbands= n_elements(bands)
;
; set up constants
flam2Jy = 3.33564095d4	; times lamda^2 in Angstroms
iFUV=0
iNUV=1
isu=2
isg=3
isr=4
isi=5
isz=6
i2J=7
i2H=8
i2K=9
iw1=10
iw2=11
iw3=12
iw4=13
waves=fltarr(nbands)		; Angstroms
bwid =fltarr(nbands)		; Angstroms
zpts =fltarr(nbands)+3630.78	; 0 mag (Jy)
flux =fltarr(nbands)-99.	; Jy
flxer=fltarr(nbands)		; Jy
extcf=fltarr(nbands)		; MW dust coefficients
;
; GALEX
waves(iFUV)= 1531.6
bwid(iFUV) = 442.4
extcf(iFUV)= 8.2
waves(iNUV)= 2256.6
bwid(iNUV) = 1059.3
extcf(iNUV)= 8.2
;
; SDSS
waves(isu) = 3543.
bwid(isu)  = 634.
extcf(isu) = 5.155
waves(isg) = 4770.
bwid(isg)  = 1409.
extcf(isg) = 3.793
waves(isr) = 6231.
bwid(isr)  = 1388
extcf(isr) = 2.751
waves(isi) = 7625.
bwid(isi)  = 1535.
extcf(isi) = 2.086
waves(isz) = 9134.
bwid(isz)  = 1409.
extcf(isz) = 1.479
;
; 2MASS
waves(i2J) = master_filter(41).mean_wave
bwid(i2J)  = 0.162*1.e4
zpts(i2J)  = zpts(i2J) * 10.^(-0.4*master_filter(41).ab_offset)
extcf(i2J) = 0.900
waves(i2H) = master_filter(42).mean_wave
bwid(i2H)  = 0.251*1.e4
zpts(i2H)  = zpts(i2H) * 10.^(-0.4*master_filter(42).ab_offset)
extcf(i2H) = 0.576
waves(i2K) = master_filter(43).mean_wave
bwid(i2K)  = 0.262*1.e4
zpts(i2K)  = zpts(i2K) * 10.^(-0.4*master_filter(43).ab_offset)
extcf(i2K) = 0.365
;
; WISE
waves(iw1) = 3.4003*1.e4
bwid(iw1)  = 0.6358*1.e4
waves(iw2) = 4.6520*1.e4
bwid(iw2)  = 1.1073*1.e4
waves(iw3) = 12.8109*1.e4
bwid(iw3)  = 6.2758*1.e4
waves(iw4) = 22.3753*1.e4
bwid(iw4)  = 4.7397*1.e4
;
q=''
;
; get list
readcol,lfile,id,ra,dec,format='a,d,d'
nloop = n_elements(ra)
;
; define photometry directories
ppath = !GLGA_ROOT+'data/'+string(floor(ra),format='(i03)')+'D/photometry/'
;
; check outputs
odir = './'
if keyword_set(ps) then $
	do_ps = (1 eq 1) $
else	do_ps = (1 eq 0)
;
if keyword_set(glgaout) then begin
	do_ps = (1 eq 1)
	odir = !GLGA_ROOT+'data/'+string(floor(ra),format='(i03)')+'D/plots/'
	do_glga = (1 eq 1)
endif else	do_glga = (1 eq 0)
;
; compress id
id = strcompress(id,/rem)
;
; loop over objects
for i=0L,nloop-1 do begin
;
; init vectors
	flux=fltarr(nbands)
	flxer=fltarr(nbands)
;
; get E(B-V)
	glactc,ra[i],dec[i],2000.,gall,galb,1,/degree
	ebmv = dust_getval(gall,galb)
;
; print status
	if not keyword_set(silent) then $
		print,i+1,'/',nloop,id[i],ra[i],dec[i],ebmv, $
			format='(i6,a,i6,2x,a,2x,2f10.5,f9.2)'
;
; loop over bands
	for j=0,nbands-1 do begin
		read_radprof,id[i],bands[j],nt_a, nt_m, nt_me, na_mu, na_mue, $
			nra, ndec, na, nb, npa, nscl, nf_m, nf_me, $
			nap_mag, nap_magerr, pathtoprofile=ppath[i], /silent
		if nt_a[0] gt 0. then begin
		    nap_mag = nap_mag[0] - ebmv * extcf[j]
		    if nap_magerr[0] ge 0. then $
			 nap_magerr = nap_magerr[0] > 0.02 $
		    else nap_magerr = nap_magerr[0]	; preserve upper limits
		    if j le isz or j ge iw1 then begin
			flux[j] = zpts[j]*10.^(-0.4*nap_mag)
			if nap_magerr ge 0. then $
				flxer[j] = flux[j]*nap_magerr / 1.0857362d0 $
			else	flxer[j] = -1.
		    endif else begin
			f=where(strcmp(master_filter.shortid,shids[j]))
			flux[j] = flux_to_apparent_mag(nap_mag, ifilter=f[0], $
				apparent_mag_err=fxe, flux_error=nap_magerr, $
				/vegasys, /reverse) * flam2Jy * waves[j]^2
			flxer[j] = fxe * flam2Jy * waves[j]^2
		    endelse
		    if not keyword_set(silent) then $
			    print,j+1,'/',nbands,bands[j],nap_mag,nap_magerr, $
			    	flux[j], flxer[j], $
				    format='(i6,a1,i6,2x,a-5,2f8.2,2g15.3)'
		endif
	endfor	; loop over bands
;
	g=where(flux gt 0., ng)
	if ng gt 0 then begin
		y0 = min(flux(g))
		y1 = max(flux(g))
		yrng = [y0*0.1,y1*10.]
	endif else yrng = [1.e-1,1.e1]
	g=where(flux ge 0. and flxer ge 0, ng)
;
; plot
	font_store=!p.font
	if do_ps then begin
		th=5
		si=1.8
		psf=id[i]+'_hSED'
		if do_glga then psf = odir[i] + psf
		psfile,psf
		print,'Plotting to: '+psf+'.ps'
		!p.font=1
	endif else begin
		th=3
		si=2.0
	endelse
	xrng = [400.,3.0e5]
	ylab = 'Host Flux (Jy)'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	a=[findgen(40)*(!pi*2/40.),0.]
	usersym,cos(a)*si,sin(a)*si,thick=th
	plot,waves,flux,psym=8,thick=th,xthick=th,ythick=th,charthi=th,$
		charsi=si,title=id[i], $
		xtitle='Wavelength (Angstroms)',xran=xrng,xsty=1, $
		ytitle=ylab,yran=yrng,/xlog,/ylog,/nodata
	if ng gt 0 then $
		oploterror,waves(g),flux(g),bwid(g)/2.,flxer(g),psym=8
;
; get limits
	u=where(flux gt 0. and flxer lt 0., nu)
	if nu gt 0 then begin
		oplot,waves(u),flux(u),psym=8
		olimplot,waves(u),flux(u),scale=2.0
	endif
;
	ci=lonarr(4)
	ci[0] = colordex('P')
	ci[1] = colordex('B')
	ci[2] = colordex('O')
	ci[3] = colordex('R')
	usersym,cos(a)*si,sin(a)*si,thick=th,/fill
	sd=[isu,isg,isr,isi,isz]
	wvsd=waves(sd)
	fxsd=flux(sd)
	oplot,wvsd,fxsd,psym=8,color=ci[1]
	gd=[iFUV,iNUV]
	wvgx=waves(gd)
	fxgx=flux(gd)
	oplot,wvgx,fxgx,psym=8,color=ci[0]
	td=[i2J,i2H,i2K]
	wv2m=waves(td)
	fx2m=flux(td)
	oplot,wv2m,fx2m,psym=8,color=ci[2]
	wd=[iw1,iw2,iw3,iw4]
	wvwd=waves(wd)
	fxwd=flux(wd)
	oplot,wvwd,fxwd,psym=8,color=ci[3]
	b=where(flux le 0., nb)
	if nb gt 0 then begin
		fbad=fltarr(nb)+2.0*10.^(!y.crange[0])
		oplot,waves[b],fbad,psym=1,thick=th,symsi=si
		b=where(fxsd le 0., nb)
		if nb gt 0 then begin
			fbad=fltarr(nb)+2.0*10.^(!y.crange[0])
			oplot,wvsd[b],fbad,psym=1,color=ci[1],thick=th,symsi=si
		endif
		b=where(fxgx le 0., nb)
		if nb gt 0 then begin
			fbad=fltarr(nb)+2.0*10.^(!y.crange[0])
			oplot,wvgx[b],fbad,psym=1,color=ci[0],thick=th,symsi=si
		endif
		b=where(fx2m le 0., nb)
		if nb gt 0 then begin
			fbad=fltarr(nb)+2.0*10.^(!y.crange[0])
			oplot,wv2m[b],fbad,psym=1,color=ci[2],thick=th,symsi=si
		endif
		b=where(fxwd le 0., nb)
		if nb gt 0 then begin
			fbad=fltarr(nb)+2.0*10.^(!y.crange[0])
			oplot,wvwd[b],fbad,psym=1,color=ci[2],thick=th,symsi=si
		endif
	endif
;
; legend
;	lem=strarr(4)
;	lem(0) = 'cz = '+string(sndat(s).cz,format='(f9.1)')
;	lem(1) = 'Str ='+string(sndat(s).smpl_str,format='(f5.2)') + $
;		' B-V ='+string(sndat(s).smpl_clr,format='(f5.2)')
;	lem[2] = 'E(B-V)!DMW!N ='+string(sndat(s).hmwebmv,format='(f5.3)')
;legend,lem,charsi=si,charthi=th,box=0,pos=[xl,0.8*10.^(!y.crange(1))]
;
	lem=strarr(4)
	lem[0] = 'GALEX'
	lem[1] = 'SDSS'
	lem[2] = '2MASS'
	lem[3] = 'WISE'
	usersym,cos(a),sin(a),thick=th,/fill
	legend,lem,psym=[8,8,8,8],color=ci,charthi=th,charsi=si,box=0
	xyouts,0.02,0.02,systime(),/norm
;
	if do_ps then $
		psclose $
	else	read,'next <cr>: ',q
	if do_glga then begin
		cmd = 'convert -rotate -90 ' + psf + '.ps GIF:' + psf + '.gif'
		if keyword_set(verbose) then print,cmd
		spawn,cmd
	endif
	!p.font=font_store
	if strupcase(strtrim(q,2)) eq 'Q' then break
;
endfor	; loop over object list
;
return
end
