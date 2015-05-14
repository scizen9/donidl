pro gx_ea_cal,std,sra,sdec,ps=ps,smoo=smoo
;+
; GX_EA_CAL - compute observed effective area for standard
;
;	gx_ea_cal,std,sra,sdec
;
; INPUTS:
;
;	std	- the name of the standard (file should be <std>_flux_1.tbl)
;	sra,sdec- the ra and dec of the standard (decimal degrees)
;
; OUTPUTS:
;
;	NONE (yet)
;
; KEYWORDS:
;
;	ps	- set to plot to postcript files
;	smoo	- smoothing length for final EA curve (default = 13 Ang)
;
; HISTORY:
;
;	10oct07 jdn	- munged from gx_std_rat
;-
; check inputs
if n_params(0) lt 3 then begin
	print,'Usage: gx_ea_cal, std, std_ra, std_dec, [, /ps, smooth_len=smooth_len]'
	return
endif
;
if keyword_set(smoo) then $
	sml = smoo $
else	sml = 13
;
; get file list
flist1=file_search('*-ng-xsp.fits', count=nf1)
flist2=file_search('*-fg-xsp.fits', count=nf2)
if nf1 le 0 or nf2 le 0 then begin
	print,'Wrong number of ng-xsp.fits and fg-xsp.fits files found'
	return
endif
;
; get standard flux
sfil = '~/ref/calib/galex/spec/'+std+'_flux_1.tbl'
if file_test(sfil) then begin
	readcol,sfil,sw,sf,sfe,sfn,s2n,form='f,f,f,f,f',/silent
endif else begin
	print,'No reference flux file found for: ',std
	return
endelse
;
; get good values
gsf = where(sf gt 0.)
;
; get current directory
cd,'./',cur=cwd
;
; get tile name
tmp=flist1(0)
tile=gettok(tmp,'-')
;if strpos(flist1(0),'_PAT_') gt 0 then begin
;	tile=strmid(tile,0,strpos(tile,'_PAT_'))
;endif else if strpos(flist1(0),'_sv') gt 0 then begin
;	tile=strmid(tile,0,strpos(tile,'_sv'))
;endif
if strpos(tile,'_00') ge 0 then $
	tile=strmid(tile,0,strpos(tile,'_00'))
openw,el,tile+'_bad_files.txt',/get_lun
;
; loop over files (NUV)
nbd1=0L
for i=0,nf1-1 do begin
	gx_dc_cnts,flist1(i),sid1,w1,c1,errs1,ra=sra,dec=sdec
	if max(c1) eq min(c1) then begin
		is_bad = (1 eq 1)
		printf,el,flist1[i]
	endif else	is_bad = (1 eq 0)
	hdr1 = headfits(flist1(i))
;
; set up arrays
	if i eq 0 then begin
		ear1 = fltarr(nf1,n_elements(c1)) ; NUV
		ero1 = fltarr(nf1,n_elements(c1))
		cnt1 = fltarr(nf1,n_elements(c1))
		err1 = fltarr(nf1,n_elements(c1))
	endif
;
; plug in arrays
	ear1(i,gsf) = c1(gsf)/sf(gsf)		; effective area (EA)
	ero1(i,gsf) = errs1(gsf)/sf(gsf)	; EA error
	cnt1(i,gsf) = c1(gsf)			; counts/s
	err1(i,gsf) = errs1(gsf)		; err in counts/s
	if is_bad then begin
		nbd1 = nbd1 + 1
		print,i+1,'  ',sid1,'  ',flist1(i),'   BAD'
	endif else $
		print,i+1,'  ',sid1,'  ',flist1(i)
endfor
;
; loop over files (FUV)
nbd2=0L
for i=0,nf2-1 do begin
	gx_dc_cnts,flist2(i),sid2,w2,c2,errs2,ra=sra,dec=sdec
	if max(c2) eq min(c2) then begin
		is_bad = (1 eq 1)
		printf,el,flist2[i]
	endif else	is_bad = (1 eq 0)
	hdr2 = headfits(flist2(i))
;
; set up arrays
	if i eq 0 then begin
		ear2 = fltarr(nf2,n_elements(c2)) ; FUV
		ero2 = fltarr(nf2,n_elements(c2))
		cnt2 = fltarr(nf2,n_elements(c2))
		err2 = fltarr(nf2,n_elements(c2))
	endif
;
; plug in arrays
	ear2(i,gsf) = c2(gsf)/sf(gsf)		; effective area (EA)
	ero2(i,gsf) = errs2(gsf)/sf(gsf)	; EA error
	cnt2(i,gsf) = c2(gsf)			; counts/s
	err2(i,gsf) = errs2(gsf)		; err in counts/s
	if is_bad then begin
		nbd2 = nbd2 + 1
		print,i+1,'  ',sid2,'  ',flist2(i),'   BAD'
	endif else $
		print,i+1,'  ',sid2,'  ',flist2(i)
endfor
free_lun,el
;
; nominal wavelength ranges
fwl = [1300,1800]
nwl = [1850,3000]
;
nuv = where(w1 gt nwl(0) and w1 lt nwl(1))
fuv = where(w2 gt fwl(0) and w2 lt fwl(1))
;
; get ea range
yrng1 = [-1,max(ear1(where(finite(ear1) eq 1)))+5.]
yrng2 = [-1,max(ear2(where(finite(ear2) eq 1)))+5.]
;
; get cnts range
crng1= [min(cnt1)>0.,max(cnt1)]
crng2= [min(cnt2)>0.,max(cnt2)]
cdel1= crng1(1) - crng1(0)
crng1= [0.0,crng1(1)+cdel1*0.3]
cdel2= crng2(1) - crng2(0)
crng2= [0.0,crng2(1)+cdel2*0.3]
;
; set up plots
font_save=!p.font
th=3
si=2.0
li=1.2
q=''
;
; get weighted mean EA and errors
;
ea1 = fltarr(n_elements(w1))	; EA
ee1 = fltarr(n_elements(w1))	; error in EA
es1 = fltarr(n_elements(w1))	; stddev in EA
cn1 = fltarr(n_elements(w1))	; counts
ec1 = fltarr(n_elements(w1))	; error in counts
ea2 = fltarr(n_elements(w2))	; EA
ee2 = fltarr(n_elements(w2))	; error in EA
es2 = fltarr(n_elements(w2))	; stddev in EA
cn2 = fltarr(n_elements(w2))	; counts
ec2 = fltarr(n_elements(w2))	; error in counts
for i=0,n_elements(w1)-1 do begin
	cvec1 = reform(cnt1(*,i))
	gd1 = where(finite(cvec1) eq 1, ngd1)
	ea1(i) = wmean(ear1(gd1,i),ero1(gd1,i))
	ee1(i) = wstdev(ear1(gd1,i),ero1(gd1,i))
	cn1(i) = wmean(cnt1(gd1,i),err1(gd1,i))
	ec1(i) = wstdev(cnt1(gd1,i),err1(gd1,i))
	cvec2 = reform(cnt2(*,i))
	gd2 = where(finite(cvec2) eq 1, ngd2)
	ea2(i) = wmean(ear2(gd2,i),ero2(gd2,i))
	ee2(i) = wstdev(ear2(gd2,i),ero2(gd2,i))
	cn2(i) = wmean(cnt2(gd2,i),err2(gd2,i))
	ec2(i) = wstdev(cnt2(gd2,i),err2(gd2,i))
	if ngd1 gt 1 then $
		es1(i) = stddev(ear1(gd1,i)) $
	else	es1(i) = ear1(0,i)
	if ngd2 gt 1 then $
		es2(i) = stddev(ear2(gd2,i)) $
	else	es2(i) = ear2(0,i)
endfor
;
; clean up
bad = where(finite(ea1) le 0, nbad)
if nbad gt 0 then begin
	ea1(bad) = 0.
	ee1(bad) = 0.
endif
bad = where(finite(ea2) le 0, nbad)
if nbad gt 0 then begin
	ea2(bad) = 0.
	ee2(bad) = 0.
endif
;
; set wl ranges
b1 = where(w1 lt 1698. or w1 gt 3100.)	; NUV range
b2 = where(w2 lt 1280. or w2 gt 1825.)	; FUV range
sea1 = ea1
sea2 = ea2
sea1(b1) = 0.
sea2(b2) = 0.
sea2=interpol(sea2,w2,w2)
;
; smooth data
;
sea1 = smooth(sea1,sml) > 0.
sea2 = smooth(sea2,sml) > 0.
;
; deal with FUV red end
b3 = where(w2 gt 1820.)
bit = smooth(sea2,125)
sea2(b3) = bit(b3) > 0.
;
; write data
;
openw,ol,tile+'_ea.tbl',/get_lun
printf,ol,'| WAVE  | FUV2   | NUV1   |'
printf,ol,'| d     | d      | d      |'
for j=0,n_elements(w1)-1 do begin
	printf,ol,w1(j),sea2(j),sea1(j), format='(f8.1,f9.4,f9.4)'
endfor
free_lun,ol
print,'Wrote file: ',tile+'_ea.tbl'
;
; plot mean EA
;
if keyword_set(ps) then begin
	psfile,tile+'_cea'
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
plot,sw,s2n,charsi=si,charthi=th,/nodata, $
	xtitle='WAVELENGTH (ANGSTROMS)',xran=[1250,3050], $
	xsty=1,xthick=th, $
	ytitle='EFFECTIVE AREA (cm!U2!N)',yran=yrng1, $
	ythick=th, ysty=1, title=tile + '  '+systime(0)
oplot,w1,ea1,color=cols(1)
oplot,w1,sea1,color=cols(1),thick=th
oplot,w1,ee1,color=colordex('green')
oplot,w1,es1,color=colordex('red')
oplot,w2,ea2,color=cols(0)
oplot,w2,sea2,color=cols(0),thick=th
oplot,w2,ee2,color=colordex('green')
oplot,w2,es2,color=colordex('red')
oplot,[nwl(0),nwl(0)],[-100,1000],thick=th,color=cols(1)
oplot,[nwl(1),nwl(1)],[-100,1000],thick=th,color=cols(1)
oplot,[fwl(0),fwl(0)],[-100,1000],thick=th,color=cols(0)
oplot,[fwl(1),fwl(1)],[-100,1000],thick=th,color=cols(0)
;
legend,['FUV','FUVsm'+strn(sml),'NUV','NUVsm'+strn(sml), $
	textoidl('\sigma'),textoidl('\sigma_{\mu}')], $
	linesty=[0,0,0,0,0,0],thick=[0,th,0,th,0,0],charsi=li,charthi=th, $
	color=[cols(0),cols(0),cols(1),cols(1), $
	colordex('red'),colordex('green')],box=0,/clear,clr_color=!p.background
legend,[strn(nf1-nbd1)+' NUV Obs',strn(nf2-nbd2)+' FUV Obs'], $
	charsi=si,charthi=th,box=0,/right
;
; plot mean Counts
;
if keyword_set(ps) then begin
	psclose
	psfile,tile+'_cint'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	!p.font=1
endif else begin
	read,'next: ',q
	if strupcase(strtrim(q,2)) eq 'Q' then return
	deepcolor
	!p.background=colordex('black')
	!p.color=colordex('white')
endelse
cols = [colordex('P'),colordex('B')]	; FUV, NUV
plot,sw,s2n,charsi=si,charthi=th,/nodata, $
	xtitle='WAVELENGTH (ANGSTROMS)',xran=[1250,3050], $
	xsty=1,xthick=th,ytitle='INTENSITY (cnts/s)',yran=crng1,$
	ythick=th, ysty=1, title=tile + '  '+systime(0)
oplot,w1,cn1,thick=th,color=cols(1)
oplot,w1,ec1,color=colordex('green')
oplot,w2,cn2,thick=th,color=cols(0)
oplot,w2,ec2,color=colordex('green')
oplot,[nwl(0),nwl(0)],[-100,1000],thick=th,color=cols(1)
oplot,[nwl(1),nwl(1)],[-100,1000],thick=th,color=cols(1)
oplot,[fwl(0),fwl(0)],[-100,1000],thick=th,color=cols(0)
oplot,[fwl(1),fwl(1)],[-100,1000],thick=th,color=cols(0)
;
legend,['FUV','NUV',textoidl('\sigma_{\mu}')], $
	linesty=[0,0,0],thick=[th,th,0],charsi=li,charthi=th, $
	color=[cols(0),cols(1),colordex('green')], $
	box=0,/clear,clr_color=!p.background
legend,[strn(nf1-nbd1)+' NUV Obs',strn(nf2-nbd2)+' FUV Obs'], $
	charsi=si,charthi=th,box=0,/right
;
; no point in these plots for one spectrum
;
if nf1 le 1 and nf2 le 1 then begin
	if keyword_set(ps) then psclose
	return
endif
;
; individual EA ratio
;
if keyword_set(ps) then begin
	psclose
	psfile,tile+'_indiv_earat'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	!p.font=1
endif else begin
	read,'next: ',q
	deepcolor
endelse
if strupcase(strtrim(q,2)) eq 'Q' then return
;
; plot effective area ratio
;
plot,sw,s2n,charsi=si,charthi=th,/nodata, $
	xtitle='WAVELENGTH (ANGSTROMS)',xran=[1250,3050], $
	xsty=1,xthick=th, $
	ytitle='EA / <EA>',yran=[0.5,1.5],$
	ythick=th, ysty=1, title=strupcase(std)
oplot,[-100,10000],[1,1],thick=th
oplot,[nwl(0),nwl(0)],[-100,1000],thick=th,color=cols(1)
oplot,[nwl(1),nwl(1)],[-100,1000],thick=th,color=cols(1)
oplot,[fwl(0),fwl(0)],[-100,1000],thick=th,color=cols(0)
oplot,[fwl(1),fwl(1)],[-100,1000],thick=th,color=cols(0)

for i=0,nf1-1 do $
	oplot,w1(nuv),ear1(i,nuv)/ea1(nuv),psym=3
for i=0,nf2-1 do $
	oplot,w2(fuv),ear2(i,fuv)/ea2(fuv),psym=3
oplot,w1(nuv),1.+(es1(nuv)/ea1(nuv)),color=cols(1)
oplot,w1(nuv),1.-(es1(nuv)/ea1(nuv)),color=cols(1)
oplot,w2(fuv),1.+(es2(fuv)/ea2(fuv)),color=cols(0)
oplot,w2(fuv),1.-(es2(fuv)/ea2(fuv)),color=cols(0)
;
legend,[textoidl('FUV \sigma'),textoidl('NUV \sigma')],box=0,$
	linesty=[0,0],color=[cols(0),cols(1)], $
	/clear,clr_color=!p.background,charsi=li
legend,[strn(nf1-nbd1)+' NUV Obs, '+strn(nf2-nbd2)+' FUV Obs',tile,systime(0),cwd], $
	box=0,/right,/clear,clr_color=!p.background,charsi=li
;
; individual counts ratio
;
if keyword_set(ps) then begin
	psclose
	psfile,tile+'_indiv_intrat'
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	!p.font=1
endif else begin
	read,'next: ',q
	deepcolor
endelse
if strupcase(strtrim(q,2)) eq 'Q' then return
;
; plot counts ratio
plot,sw,s2n,charsi=si,charthi=th,/nodata, $
	xtitle='WAVELENGTH (ANGSTROMS)',xran=[1250,3050], $
	xsty=1,xthick=th,ytitle='INT / <INT>',yran=[0.5,1.5],$
	ythick=th, ysty=1, title=strupcase(std)
oplot,[-100,10000],[1,1],thick=th
oplot,[nwl(0),nwl(0)],[-100,1000],thick=th,color=cols(1)
oplot,[nwl(1),nwl(1)],[-100,1000],thick=th,color=cols(1)
oplot,[fwl(0),fwl(0)],[-100,1000],thick=th,color=cols(0)
oplot,[fwl(1),fwl(1)],[-100,1000],thick=th,color=cols(0)

for i=0,nf1-1 do $
	oplot,w1(nuv),cnt1(i,nuv)/cn1(nuv),psym=3
for i=0,nf2-1 do $
	oplot,w2(fuv),cnt2(i,fuv)/cn2(fuv),psym=3
oplot,w1(nuv),1.+(ec1(nuv)/cn1(nuv)),color=cols(1)
oplot,w1(nuv),1.-(ec1(nuv)/cn1(nuv)),color=cols(1)
oplot,w2(fuv),1.+(ec2(fuv)/cn2(fuv)),color=cols(0)
oplot,w2(fuv),1.-(ec2(fuv)/cn2(fuv)),color=cols(0)
;
legend,[textoidl('FUV \sigma_{\mu}'),textoidl('NUV \sigma_{\mu}')],box=0,$
	linesty=[0,0],color=[cols(0),cols(1)], $
	/clear,clr_color=!p.background,charsi=li
legend,[strn(nf1-nbd1)+' NUV Obs, '+strn(nf2-nbd2)+' FUV Obs',tile,systime(0),cwd], $
	box=0,/right,/clear,clr_color=!p.background,charsi=li
if keyword_set(ps) then $
	psclose
;
!p.font=font_save
;
return
end
