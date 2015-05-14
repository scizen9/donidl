pro gx_std_rat,std,sra,sdec,ps=ps,groupall=groupall
;+
; GX_STD_RAT - compute ratio between standard and observed flux
;
;	gx_std_rat,std,sra,sdec
;
; INPUTS:
;
;	std	- the name of the standard (file should be <std>_flux.tbl)
;	sra,sdec- the ra and dec of the standard (decimal degrees)
;
; KEYWORDS:
;
;	ps	- set to generate postscript file
;	groupall - set to use all files in a group
;
; OUTPUTS:
;
;	NONE (yet)
;
; HISTORY:
;
;	19sep07 jdn	- initial revision
;	20sep07 jdn	- added statistics and plot annotations
;-
; check inputs
if n_params(0) lt 3 then begin
	print,'Usage: gx_std_rat, std, std_ra, std_dec, [, /ps]'
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
; get current directory
cd,'./',cur=cwd
;
; do we have sub-visits?
svp = strpos(flist(0),'_sv')
if svp ge 0 then begin
	trim = 10
	t = where(strpos(flist,'_sv01') ge 0, ngrp)	; # of plot groups
	if keyword_set(groupall) then ngrp = 1
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
	svp = strpos(flist[i],'_sv')
	gx_read_gsp,flist(i),sid,f,fe,w,dga,exptime,xorms,yorms,mdres, $
		ra=sra,dec=sdec
;
; set up arrays
	if i eq 0 then begin
		far = fltarr(nf,n_elements(f))
		fear= fltarr(nf,n_elements(f))
		rar = fltarr(nf,n_elements(f))
		rnar= fltarr(nf,n_elements(f))
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
	far(i,*)  = f(*)
	fear(i,*) = fe(*)
	rar(i,*)  = f(*)/sf(*)
	rnar(i,*) = f(*)/sfn(*)
	dgar(i)   = dga
        ext(i)    = exptime
	xfr(i)    = xorms
	mdr(i)    = mdres
	sids(i)   = sid
	if svp ge 0 then begin
		vis(i)  = fix(strmid(flist(i),svp-4,4))
		svar(i)	= fix(strmid(flist(i),svp+3,2))
		if keyword_set(groupall) then svar[i] = 0
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
nwl = [1850,3000]
fwl = [1300,1800]
nuv = where(w gt nwl(0) and w lt nwl(1))
fuv = where(w gt fwl(0) and w lt fwl(1))
;
; where are we above s2n of 5?
ngo = where(s2n(nuv) gt 5.,nngo)
fgo = where(s2n(fuv) gt 5.,nfgo)
;
; re-set wavelength ranges
nwl = [w(nuv(ngo(0))), w(nuv(ngo(nngo-1)))]
fwl = [w(fuv(fgo(0))), w(fuv(fgo(nfgo-1)))]
nuv = where(w gt nwl(0) and w lt nwl(1))
fuv = where(w gt fwl(0) and w lt fwl(1))
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
	    psf = tile+'_all_rat'
    endif else begin
	    np = where(vis eq uvis(j),nnp)
	    tlab = strupcase(std) + ' VISIT '+string(uvis(j),form='(I04)')
	    vlab = strn(nnp) + ' SUB-VISITS'
	    psf = tile+'_'+string(uvis(j),form='(I04)')+'_all_rat'
    endelse
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
    rr = fltarr(nnp,n_elements(w))
    rmspc = fltarr(n_elements(w))
    for i=0,nnp-1 do begin
	rr(i,*) = rar(np(i),*)
	if i eq 0 then begin
		plot,w,rar(np(i),*), charsi=si,charthi=th, $
			xtitle='WAVELENGTH (ANGSTROMS)',xran=[1250,3050], $
			xsty=1,xthick=th, $
			ytitle='GALEX / STIS FLUX',yran=[-0.1,1.99],$
			ythick=th, ysty=1, title=tlab
		oplot,[0,10000],[1,1],linesty=2,thick=2
		oplot,[nwl(0),nwl(0)],[-100,100],thick=th,color=cols(1)
		oplot,[nwl(1),nwl(1)],[-100,100],thick=th,color=cols(1)
		oplot,[fwl(0),fwl(0)],[-100,100],thick=th,color=cols(0)
		oplot,[fwl(1),fwl(1)],[-100,100],thick=th,color=cols(0)
	endif else begin
		oplot,w,rar(np(i),*)
	endelse
    endfor
;
; calculate statistics
    if nnp ge 3 then begin
	for i=0,n_elements(w)-1 do $
	    rmspc(i) = stddev(rr(*,i))
        oplot,w,rmspc,color=colordex('green')
	oplot,[0,1e6],[0,0],linesty=1
    endif
;
    mystats,rr(*,nuv),nmn,nsg
    mystats,rr(*,fuv),fmn,fsg
    xyouts,2250,-0.07,'<NUV> = '+string(nmn*100.,form='(f5.1)')+' +- '+ $
	string(nsg*100.,form='(f5.1)')+' %',charsi=li,color=cols(1)
    xyouts,1335,-0.07,'<FUV> = '+string(fmn*100.,form='(f5.1)')+' +- '+ $
	string(fsg*100.,form='(f5.1)')+' %',charsi=li,color=cols(0)
    legend,[vlab,''],box=0,charsi=si
    legend,[cwd,systime(0)],box=0,/right, /clear,clr_color=!p.background, $
	charsi=li
;
    if keyword_set(ps) then $
	psclose $
    else read,'next: ',q
endfor
;
; individual plots
;
; loop
for i=0,nf-1 do begin
;
; ps output
	if keyword_set(ps) then begin
		tmp=flist(i)
		pfl=gettok(tmp,'.')+'_rat'
		psfile,pfl,/color
		deepcolor
		!p.background=colordex('white')
		!p.color=colordex('black')
		!p.font=1
	endif else deepcolor
;
	plot,w,rar(i,*), charsi=si,charthi=th, $
		xtitle='WAVELENGTH (ANGSTROMS)',xran=[1250,3050], $
		xsty=1,xthick=th, $
		ytitle='GALEX / STIS FLUX',yran=[-0.1,1.99],$
		ythick=th, ysty=1, title=strupcase(std)
	oplot,[0,10000],[1,1],linesty=2,thick=2
	oplot,[nwl(0),nwl(0)],[-100,100],thick=th,color=cols(1)
	oplot,[nwl(1),nwl(1)],[-100,100],thick=th,color=cols(1)
	oplot,[fwl(0),fwl(0)],[-100,100],thick=th,color=cols(0)
	oplot,[fwl(1),fwl(1)],[-100,100],thick=th,color=cols(0)
;
; individual stats
	mystats,rar(i,nuv),nmn,nsg
	mystats,rar(i,fuv),fmn,fsg
	xyouts,2250,0,'<NUV> = '+string(nmn*100.,form='(f5.1)')+' +- '+ $
		string(nsg*100.,form='(f5.1)')+' %',charsi=li,color=cols(1)
	xyouts,1335,0,'<FUV> = '+string(fmn*100.,form='(f5.1)')+' +- '+ $
		string(fsg*100.,form='(f5.1)')+' %',charsi=li,color=cols(0)
	legend,[flist(i),'DET GRISM ANG = '+string(dgar(i),form='(f5.1)'), $
		'EXPOSURE TIME = '+string(ext(i),form='(f7.1)'), $
		'X OFF RMS = '+string(xfr(i),form='(f5.2)'), $
		'MEDN RESID = '+string(mdr(i),form='(f5.2)')], $
		box=0,/clear,clr_color=!p.background
	legend,[cwd,systime(0),'SPEC_ID ='+strn(sids(i))],box=0,/right, $
		/clear,clr_color=!p.background,charsi=li
	if keyword_set(ps) then $
		psclose $
	else	read,'next: ',q
endfor
;
!p.font=font_save
;
return
end
