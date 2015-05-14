pro gx_ea_test,tfile,std,sra,sdec,ps=ps
;+
; GX_EA_TEST - test effective area 
;
;	gx_ea_test,tfile,std,sra,sdec
;
; INPUTS:
;
;	tfile	- test ea file name
;	std	- the name of the standard (file should be <std>_flux.tbl)
;	sra,sdec- the ra and dec of the standard (decimal degrees)
;
; OUTPUTS:
;
;	NONE (yet)
;
; HISTORY:
;
;	10oct07 jdn	- munged from gx_std_rat
;-
; check inputs
if n_params(0) lt 3 then begin
	print,'Usage: gx_ea_test, test_ea_file, std, std_ra, std_dec, [, /ps]'
	return
endif
;
; get file list
flist=file_search('*xg-gsp.fits', count=nf)
if nf le 0 then begin
	print,'No -xg-gsp.fits files found'
	return
endif
;
; get standard flux
sfil = '~/ref/calib/galex/spec/'+std+'_flux.tbl'
if file_test(sfil) then begin
	readcol,sfil,sw,sf,sfe,sfn,s2n,form='f,f,f,f,f',/silent
endif else begin
	print,'No reference flux file found for: ',std
	return
endelse
;
; get EA curves
gx_read_ea,tfea,tfw,tnea,tnw,test=tfile
gx_read_ea,fea,fw,nea,nw	; nominal set
;
; get current directory
cd,'./',cur=cwd
;
; do we have sub-visits?
svp = strpos(flist(0),'_sv')
if svp ge 0 then begin
	trim = 10
	t = where(strpos(flist,'_sv01') ge 0, ngrp)	; # of plot groups
endif else begin
	trim = 5
	ngrp = 1	; one group to plot
endelse
;
; get tile name
tmp=flist(0)
tile=gettok(tmp,'-')
tile=strmid(tile,0,strlen(tile)-trim)	; trim visit number (and maybe subvisit)
;
; loop over files
for i=0,nf-1 do begin
	gx_read_gsp,flist(i),sid,f,fe,w,dga,exptime,xorms,yorms,mdres, $
		ra=sra,dec=sdec
;
; set up arrays
	if i eq 0 then begin
		fear= fltarr(nf,n_elements(f))
		near= fltarr(nf,n_elements(f))
		fcar= fltarr(nf,n_elements(f))
		ncar= fltarr(nf,n_elements(f))
		fflx= fltarr(nf,n_elements(f))
		nflx= fltarr(nf,n_elements(f))
		dgar= fltarr(nf)
		ext = fltarr(nf)
		xfr = fltarr(nf)
		mdr = fltarr(nf)
		sids= lonarr(nf)
		svar= intarr(nf)
		vis = intarr(nf)
	endif
;
; plug in arrays
	fcnt	= f * interpol(fea,fw,w)
	ncnt	= f * interpol(nea,nw,w)
	fcar(i,*) = fcnt(*)
	ncar(i,*) = ncnt(*)
	fear(i,*) = fcnt(*)/sf(*)
	near(i,*) = ncnt(*)/sf(*)
	ttfea = interpol(tfea,tfw,w)
	ttnea = interpol(tnea,tnw,w)
	fflx(i,*) = fcnt(*)/ttfea(*)
	nflx(i,*) = ncnt(*)/ttnea(*)
	dgar(i)	= dga
        ext(i)	= exptime
	xfr(i)	= xorms
	mdr(i)	= mdres
	sids(i)	= sid
	if svp ge 0 then begin
		vis(i)  = fix(strmid(flist(i),svp-4,4))
		svar(i)	= fix(strmid(flist(i),svp+3,2))
	endif else begin
		vis(i)  = fix(strmid(flist(i),strpos(flist(i),'-')-4,4))
		svar(i) = 0
	endelse
	print,i+1,'  ',sid,'  ',flist(i)
endfor
;
; uniqe visits
tmp=vis(sort(vis))
uvis=tmp(uniq(vis))
;
; nominal wavelength ranges
fwl = [1350,1800]
nwl = [1850,3000]
fuv = where(w gt fwl(0) and w lt fwl(1))
nuv = where(w gt nwl(0) and w lt nwl(1))
;
; get ea range
yrng = [-1,max([fear,near])+5.]
;
; get cnts range
yrng2= [min([fcar,ncar])>0.,max([fcar,ncar])]
ydel = yrng2(1) - yrng2(0)
yrng2= [0.0,yrng2(1)+ydel*0.2]
;
; get flux range
mxflx = max(sf([fuv,nuv]))
mxflx = mxflx + mxflx * 0.45
yrng3 = [mxflx*(-0.05),mxflx]
;
; set up plots
font_save=!p.font
th=3
si=2.0
li=1.2
q=''
;
; plot data groups
;
for j=0,ngrp-1 do begin
    if ngrp eq 1 then begin
	    np = where(svar eq 0, nnp)
	    tlab = strupcase(std)
	    vlab = strn(nnp) + ' VISITS'
	    psf = tile+'_all_tea'
	    psc = tile+'_all_tcnts'
	    pss = tile+'_all_tflux'
    endif else begin
	    np = where(vis eq uvis(j),nnp)
	    tlab = strupcase(std) + ' VISIT '+string(uvis(j),form='(I04)')
	    vlab = strn(nnp) + ' SUB-VISITS'
	    psf = tile+'_'+string(uvis(j),form='(I04)')+'_all_tea'
	    psc = tile+'_'+string(uvis(j),form='(I04)')+'_all_tcnts'
	    pss = tile+'_'+string(uvis(j),form='(I04)')+'_all_tflux'
    endelse
;
; start with effective area plots
;
    if keyword_set(ps) then begin
	psfile,psf
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	!p.font=1
    endif else begin
	deepcolor
	!p.background=colordex('black')
	!p.color=colordex('white')
    endelse
    cols = [colordex('P'),colordex('B')]	; FUV, NUV
;
; accumulate statistics
    frr = fltarr(nnp,n_elements(w))
    nrr = fltarr(nnp,n_elements(w))
    frms = fltarr(n_elements(w))
    nrms = fltarr(n_elements(w))
    for i=0,nnp-1 do begin
	frr(i,*) = fear(np(i),*)
	nrr(i,*) = near(np(i),*)
	if i eq 0 then begin
		plot,sw,s2n,charsi=si,charthi=th,/nodata, $
			xtitle='WAVELENGTH (ANGSTROMS)',xran=[1250,3050], $
			xsty=1,xthick=th, $
			ytitle='EFFECTIVE AREA (cm!U2!N)',yran=yrng,$
			ythick=th, ysty=1, title=tlab
		oplot,tfw,tfea,thick=th,color=cols(0)
		oplot,tnw,tnea,thick=th,color=cols(1)
		oplot,w(fuv),fear(np(i),fuv)
		oplot,w(nuv),near(np(i),nuv)
		oplot,[nwl(0),nwl(0)],[-100,100],thick=th,color=cols(1)
		oplot,[nwl(1),nwl(1)],[-100,100],thick=th,color=cols(1)
		oplot,[fwl(0),fwl(0)],[-100,100],thick=th,color=cols(0)
		oplot,[fwl(1),fwl(1)],[-100,100],thick=th,color=cols(0)
	endif else begin
		oplot,w(fuv),fear(np(i),fuv)
		oplot,w(nuv),near(np(i),nuv)
	endelse
    endfor
;
; calculate statistics
    if nnp ge 3 then begin
	for i=0,n_elements(w)-1 do begin
	    frms(i) = stddev(frr(*,i))
	    nrms(i) = stddev(nrr(*,i))
	endfor
        oplot,w(fuv),frms(fuv),color=colordex('green')
        oplot,w(nuv),nrms(nuv),color=colordex('green')
    endif
;
    legend,[vlab,''],box=0,charsi=si
    legend,[cwd,tfile,systime(0)],box=0,/right,/clear,clr_color=!p.background, $
	charsi=li
;
    if keyword_set(ps) then $
	psclose $
    else read,'next: ',q
    if strupcase(strtrim(q,2)) eq 'Q' then return
;
; now plot counts
;
    if keyword_set(ps) then begin
	psfile,psc
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	!p.font=1
    endif else begin
	deepcolor
	!p.background=colordex('black')
	!p.color=colordex('white')
    endelse
    cols = [colordex('P'),colordex('B')]	; FUV, NUV
;
; accumulate statistics
    frr = fltarr(nnp,n_elements(w))
    nrr = fltarr(nnp,n_elements(w))
    frms = fltarr(n_elements(w))
    nrms = fltarr(n_elements(w))
    for i=0,nnp-1 do begin
	frr(i,*) = fcar(np(i),*)
	nrr(i,*) = ncar(np(i),*)
	if i eq 0 then begin
		plot,sw,s2n,charsi=si,charthi=th,/nodata, $
			xtitle='WAVELENGTH (ANGSTROMS)',xran=[1250,3050], $
			xsty=1,xthick=th,ytitle='COUNTS',yran=yrng2,$
			ythick=th, ysty=1, title=tlab
		oplot,w(fuv),fcar(np(i),fuv)
		oplot,w(nuv),ncar(np(i),nuv)
		oplot,[nwl(0),nwl(0)],[-100,100],thick=th,color=cols(1)
		oplot,[nwl(1),nwl(1)],[-100,100],thick=th,color=cols(1)
		oplot,[fwl(0),fwl(0)],[-100,100],thick=th,color=cols(0)
		oplot,[fwl(1),fwl(1)],[-100,100],thick=th,color=cols(0)
	endif else begin
		oplot,w(fuv),fcar(np(i),fuv)
		oplot,w(nuv),ncar(np(i),nuv)
	endelse
    endfor
;
; calculate statistics
    if nnp ge 3 then begin
	for i=0,n_elements(w)-1 do begin
	    frms(i) = stddev(frr(*,i))
	    nrms(i) = stddev(nrr(*,i))
	endfor
        oplot,w(fuv),frms(fuv),color=colordex('green')
        oplot,w(nuv),nrms(nuv),color=colordex('green')
    endif
;
    legend,[vlab,''],box=0,charsi=si
    legend,[cwd,tfile,systime(0)],box=0,/right,/clear,clr_color=!p.background, $
	charsi=li
;
    if keyword_set(ps) then $
	psclose $
    else read,'next: ',q
    if strupcase(strtrim(q,2)) eq 'Q' then return
;
; now plot flux
;
    if keyword_set(ps) then begin
	psfile,pss
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	!p.font=1
    endif else begin
	deepcolor
	!p.background=colordex('black')
	!p.color=colordex('white')
    endelse
    cols = [colordex('P'),colordex('B')]	; FUV, NUV
;
; accumulate statistics
    frr = fltarr(nnp,n_elements(w))
    nrr = fltarr(nnp,n_elements(w))
    frms = fltarr(n_elements(w))
    nrms = fltarr(n_elements(w))
    fmen = fltarr(n_elements(w))
    nmen = fltarr(n_elements(w))
    for i=0,nnp-1 do begin
	frr(i,*) = fflx(np(i),*)
	nrr(i,*) = nflx(np(i),*)
	if i eq 0 then begin
		plot,sw,s2n,charsi=si,charthi=th,/nodata, $
			xtitle='WAVELENGTH (ANGSTROMS)',xran=[1250,3050], $
			xsty=1,xthick=th, $
			ytitle='FLUX (pho/sec/cm!U2!N/Ang)',yran=yrng3,$
			ythick=th, ysty=1, title=tlab
		oplot,w(fuv),fflx(np(i),fuv)
		oplot,w(nuv),nflx(np(i),nuv)
		oplot,[nwl(0),nwl(0)],[-100,100],thick=th,color=cols(1)
		oplot,[nwl(1),nwl(1)],[-100,100],thick=th,color=cols(1)
		oplot,[fwl(0),fwl(0)],[-100,100],thick=th,color=cols(0)
		oplot,[fwl(1),fwl(1)],[-100,100],thick=th,color=cols(0)
	endif else begin
		oplot,w(fuv),fflx(np(i),fuv)
		oplot,w(nuv),nflx(np(i),nuv)
	endelse
    endfor
;
; calculate statistics
    if nnp ge 3 then begin
	for i=0,n_elements(w)-1 do begin
	    fmo = moment(frr(*,i),sdev=fsig,/double)
	    nmo = moment(nrr(*,i),sdev=nsig,/double)
	    frms(i) = fsig
	    nrms(i) = nsig
	    fmen(i) = fmo(0)
	    nmen(i) = nmo(0)
	endfor
        oplot,w(fuv),frms(fuv),color=colordex('green')
        oplot,w(nuv),nrms(nuv),color=colordex('green')
	sfmen = smooth(fmen(fuv),25,missing=0.,/nan)
	snmen = smooth(nmen(nuv),25,missing=0.,/nan)
	fmen(fuv) = sfmen(*)
	nmen(nuv) = snmen(*)
	oplot,w(fuv),fmen(fuv),color=cols(0),thick=th*2.
	oplot,w(nuv),nmen(nuv),color=cols(1),thick=th*2.
    endif
;
; plot standard star
    oplot,sw,sf,color=colordex('red'),thick=th
;
    legend,[vlab,''],box=0,charsi=si
    legend,[cwd,tfile,systime(0)],box=0,/right,/clear,clr_color=!p.background, $
	charsi=li
;
    if keyword_set(ps) then $
	psclose $
    else read,'next: ',q
    if strupcase(strtrim(q,2)) eq 'Q' then return
endfor
;
; individual plots
;
; loop
for i=0,nf-1 do begin
;
; write out ea data
	if keyword_set(ps) then begin
		tmp = flist(i)
		rute = gettok(tmp,'.')
		pfl = rute + '_tea'
		pfc = rute + '_tcnts'
		pff = rute + '_tflux'
		psfile,pfl,/color
		deepcolor
		!p.background=colordex('white')
		!p.color=colordex('black')
		!p.font=1
	endif else deepcolor
;
; plot effective area
;
	plot,sw,s2n,charsi=si,charthi=th,/nodata, $
		xtitle='WAVELENGTH (ANGSTROMS)',xran=[1250,3050], $
		xsty=1,xthick=th, $
		ytitle='EFFECTIVE AREA (cm!U2!N)',yran=yrng,$
		ythick=th, ysty=1, title=strupcase(std)
	oplot,tfw,tfea,thick=th,color=cols(0)
	oplot,tnw,tnea,thick=th,color=cols(1)
	oplot,w(fuv),fear(i,fuv)
	oplot,w(nuv),near(i,nuv)
	oplot,[nwl(0),nwl(0)],[-100,100],thick=th,color=cols(1)
	oplot,[nwl(1),nwl(1)],[-100,100],thick=th,color=cols(1)
	oplot,[fwl(0),fwl(0)],[-100,100],thick=th,color=cols(0)
	oplot,[fwl(1),fwl(1)],[-100,100],thick=th,color=cols(0)
;
	legend,[flist(i),'DET GRISM ANG = '+string(dgar(i),form='(f5.1)'), $
		'EXPOSURE TIME = '+string(ext(i),form='(f7.1)'), $
		'X OFF RMS = '+string(xfr(i),form='(f5.2)'), $
		'MEDN RESID = '+string(mdr(i),form='(f5.2)')], $
		box=0,/clear,clr_color=!p.background
	legend,[cwd,tfile,systime(0),'SPEC_ID ='+strn(sids(i))],box=0,/right, $
		/clear,clr_color=!p.background,charsi=li
	if keyword_set(ps) then $
		psclose $
	else	read,'next: ',q
    	if strupcase(strtrim(q,2)) eq 'Q' then return
;
; now plot counts
;
	if keyword_set(ps) then begin
		psfile,pfc
		deepcolor
		!p.background=colordex('white')
		!p.color=colordex('black')
		!p.font=1
	endif else deepcolor
	plot,sw,s2n,charsi=si,charthi=th,/nodata, $
		xtitle='WAVELENGTH (ANGSTROMS)',xran=[1250,3050], $
		xsty=1,xthick=th,ytitle='COUNTS',yran=yrng2,$
		ythick=th, ysty=1, title=strupcase(std)
	oplot,w(fuv),fcar(i,fuv)
	oplot,w(nuv),ncar(i,nuv)
	oplot,[nwl(0),nwl(0)],[-100,100],thick=th,color=cols(1)
	oplot,[nwl(1),nwl(1)],[-100,100],thick=th,color=cols(1)
	oplot,[fwl(0),fwl(0)],[-100,100],thick=th,color=cols(0)
	oplot,[fwl(1),fwl(1)],[-100,100],thick=th,color=cols(0)
;
	legend,[flist(i),'DET GRISM ANG = '+string(dgar(i),form='(f5.1)'), $
		'EXPOSURE TIME = '+string(ext(i),form='(f7.1)'), $
		'X OFF RMS = '+string(xfr(i),form='(f5.2)'), $
		'MEDN RESID = '+string(mdr(i),form='(f5.2)')], $
		box=0,/clear,clr_color=!p.background
	legend,[cwd,tfile,systime(0),'SPEC_ID ='+strn(sids(i))],box=0,/right, $
		/clear,clr_color=!p.background,charsi=li
	if keyword_set(ps) then $
		psclose $
	else	read,'next: ',q
    	if strupcase(strtrim(q,2)) eq 'Q' then return
;
; now plot flux
;
	if keyword_set(ps) then begin
		psfile,pff
		deepcolor
		!p.background=colordex('white')
		!p.color=colordex('black')
		!p.font=1
	endif else deepcolor
	plot,sw,s2n,charsi=si,charthi=th,/nodata, $
		xtitle='WAVELENGTH (ANGSTROMS)',xran=[1250,3050], $
		xsty=1,xthick=th, $
		ytitle='FLUX (pho/sec/cm!U2!N/Ang)',yran=yrng3,$
		ythick=th, ysty=1, title=strupcase(std)
	oplot,w(fuv),fflx(i,fuv)
	oplot,w(nuv),nflx(i,nuv)
	oplot,[nwl(0),nwl(0)],[-100,100],thick=th,color=cols(1)
	oplot,[nwl(1),nwl(1)],[-100,100],thick=th,color=cols(1)
	oplot,[fwl(0),fwl(0)],[-100,100],thick=th,color=cols(0)
	oplot,[fwl(1),fwl(1)],[-100,100],thick=th,color=cols(0)
;
	legend,[flist(i),'DET GRISM ANG = '+string(dgar(i),form='(f5.1)'), $
		'EXPOSURE TIME = '+string(ext(i),form='(f7.1)'), $
		'X OFF RMS = '+string(xfr(i),form='(f5.2)'), $
		'MEDN RESID = '+string(mdr(i),form='(f5.2)')], $
		box=0,/clear,clr_color=!p.background
	legend,[cwd,tfile,systime(0),'SPEC_ID ='+strn(sids(i))],box=0,/right, $
		/clear,clr_color=!p.background,charsi=li
;
; plot standard star
	oplot,sw,sf,color=colordex('red'),thick=th
;
	if keyword_set(ps) then $
		psclose $
	else	read,'next: ',q
    	if strupcase(strtrim(q,2)) eq 'Q' then return
endfor
;
!p.font=font_save
;
return
end
