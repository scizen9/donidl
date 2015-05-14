pro gx_std_avg,ps=ps,image=image
;+
; GX_STD_AVG - get average comparison for set of observations
;
;	gx_std_avg,/ps,/image
;
; KEYWORDS:
;
;	ps    - output postscript file
;	image - set to enable plotting image strips on plot
;
; OUTPUTS:
;
;	NONE (yet)
;
; HISTORY:
;
;	20sep07 jdn	- initial revision
;	24sep07 jdn	- now checks for ref flux file, uses standard ra,dec
;	27sep07 jdn	- plot restructuring
;	02jun10 jdn	- now use file for input
;-
; get standard info
stdf = file_search('*-std.txt',count=nf)
if nf ne 1 then begin
	print,'GX_STD_AVG - Error: wrong number of *-std.txt files found: ',nf
	return
endif
readcol,stdf[0],std,sra,sdec,format='a,d,d',/silent,comment='#'
std=std[0] & sra=sra[0] & sdec=sdec[0]
;
; get obs list
obsf = file_search('*-obs.txt',count=nf)
if nf ne 1 then begin
	print,'GX_STD_AVG - Error: wrong number of obs list files found: ',nf
	return
endif
readcol,obsf[0],sv,paths,format='a,a',/silent,comment='#'
npth = n_elements(paths)
;
; get standard flux
sfil = '~/ref/calib/galex/spec/'+std+'_flux.tbl'
if file_test(sfil) then begin
	readcol,sfil[0],sw,sf,sfe,sfn,s2n,form='f,f,f,f,f',/silent
endif else begin
	print,'No reference flux file found for: ',std
	return
endelse
;
; set up arrays
far = fltarr(n_elements(sf))
fear= fltarr(n_elements(sf))
rar = fltarr(n_elements(sf))
rnar= fltarr(n_elements(sf))
gs2n= fltarr(n_elements(sf))
dgar= 0.
ext = 0.
xfr = 0.
mdr = 0.
;
; set up plots
font_save=!p.font
deepcolor
!p.background=colordex('black')
!p.color=colordex('white')
th=3
si=2.0
li=1.2
q=''
;
; open log file
logf='std_avg.dat'
openw,ol,logf,/get_lun
printf,ol,'#',sfil[0],npth,std,systime(0),form='(a1,1x,a,2x,i6,2x,a,2x,a)'
printf,ol,'#  EXP    DGRA    XRMS  MDRS    <FUV>   sFUV  <NUV>   sNUV'
;
; loop over paths
nav = 0L
for k=0,npth-1 do begin
;
; get file list
    if strcmp(sv[k],'00') eq 1 then $
	    svspec = '' $
    else    svspec = '_sv'+sv[k]
    flist=file_search(paths[k]+'/*'+svspec+'-xg-gsp.fits', count=nf)
    if nf ne 1 then begin
	print,'GX_STD_AVG - Error: no data for: ',sv[k],'  ',paths[k]
    endif else begin
;
; read in data
	gx_read_gsp,flist(0),sid,f,fe,w,dga,exptime,xorms,yorms,mdres, $
		visit,subvis,ra=sra,dec=sdec
;
; plug in arrays
	far	= far + f
	fear	= fear + fe
	rar	= rar + f/sf
	rnar	= rnar + f/sfn
	gs2n	= gs2n + f/fe
	dgar	= dgar + dga
	ext	= ext + exptime
	xfr	= xfr + xorms
	mdr	= mdr + mdres
	print,visit,subvis,sid, form='(a,2x,a,2x,i)'
	nav	= nav + 1L
    endelse
endfor
;
; avg values
far	= far / float(nav)
fear	= fear / float(nav)
rar	= rar / float(nav)
rnar	= rnar / float(nav)
gs2n	= gs2n / float(nav)
dgar	= dgar / float(nav)
xfr	= xfr / float(nav)
mdr	= mdr / float(nav)
;
; nominal wavelength ranges
nwl = [1850,3000]
fwl = [1300,1800]
nuv = where(w gt nwl(0) and w lt nwl(1))
fuv = where(w gt fwl(0) and w lt fwl(1))
;
; where are we above ref s2n?
rfs2n = 10.
ngo = where(gs2n(nuv) gt rfs2n, nngo)
if nngo le 1 then $
	ngo = where(s2n(nuv) gt rfs2n, nngo)
fgo = where(gs2n(fuv) gt rfs2n, nfgo)
if nfgo le 1 then $
	fgo = where(s2n(fuv) gt rfs2n, nfgo)
;
; re-set wavelength ranges
nwl = [w(nuv(ngo(0))), w(nuv(ngo(nngo-1)))]
fwl = [w(fuv(fgo(0))), w(fuv(fgo(nfgo-1)))]
tnuv = where(w gt nwl(0) and w lt nwl(1),ntnuv)
if ntnuv gt 3 then nuv = tnuv
tfuv = where(w gt fwl(0) and w lt fwl(1),ntfuv)
if ntfuv gt 3 then fuv = tfuv
;
; get flux range
mxflx = max(sf([fuv,nuv]))
mxflx = mxflx + mxflx * 0.45
yrng = [mxflx*(-0.05),mxflx]
ydel = yrng(1) - yrng(0)
imhi=800.
;
; plot average of observations
pos = [0.13,0.15,0.98,0.92]
;
; ps output
if keyword_set(ps) then begin
	psfile,'std_avcomp',/color
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	!p.font=1
endif else deepcolor
cols = [colordex('P'),colordex('B')]	; FUV, NUV
;
plot,sw,sf, charsi=si,charthi=th,linesty=0,thick=th,/nodata, $
	xtitle='WAVELENGTH (ANGSTROMS)',xran=[1250,3050], $
	xsty=1,xthick=th, $
	ytitle='<FLUX> (pho/sec/cm!U2!N/Ang)', yran=yrng,$
	ythick=th, ysty=1, title=strupcase(std), $
	position=pos
oplot,sw,s2n/10000.,color=colordex('A'),thick=th
oplot,sw,sfn,color=colordex('green')
oplot,sw,sf,color=colordex('red'),thick=th
oplot,w,far
oplot,w,gs2n/10000.,color=colordex('O'),thick=th
oplot,[-1.e9,1.e9],[rfs2n,rfs2n]/10000.,linesty=2
oplot,[1850,1850],[-100,100],color=cols(1)
oplot,[3000,3000],[-100,100],color=cols(1)
oplot,[1300,1300],[-100,100],color=cols(0)
oplot,[1800,1800],[-100,100],color=cols(0)
oplot,[nwl(0),nwl(0)],[-100,100],thick=th,color=cols(1)
oplot,[nwl(1),nwl(1)],[-100,100],thick=th,color=cols(1)
oplot,[fwl(0),fwl(0)],[-100,100],thick=th,color=cols(0)
oplot,[fwl(1),fwl(1)],[-100,100],thick=th,color=cols(0)
;
; stats
mystats,rar(nuv),nmn,nsg
mystats,rar(fuv),fmn,fsg
printf,ol,ext,dgar,xfr,mdr,fmn*100.,fsg*100.,nmn*100.,nsg*100., $
	format='(f8.1,2x,3f6.2,2x,4f7.1)'
print,ext,dgar,xfr,mdr,fmn*100.,fsg*100.,nmn*100.,nsg*100., $
	format='(f8.1,2x,3f6.2,2x,4f7.1)'
xyouts,2250,-0.001,'<NUV> = '+string(nmn*100.,form='(f5.1)')+ $
	' +- '+ string(nsg*100.,form='(f5.1)')+' %', $
	charsi=li,color=cols(1)
xyouts,1345,-0.001,'<FUV> = '+string(fmn*100.,form='(f5.1)')+ $
	' +- '+ string(fsg*100.,form='(f5.1)')+' %', $
	charsi=li,color=cols(0)
legend,['<DET GRISM ANG> = '+string(dgar,form='(f5.1)'), $
	'EXPOSERE TIME = '+string(ext,form='(f7.1)'), $
	'<X OFF RMS> = '+string(xfr,form='(f5.2)'), $
	'<MEDN RESID> = '+string(mdr,form='(f5.2)'), $
	'NOBS = '+string(nav)], $
	box=0,/clear,clr_color=!p.background
xyouts,1250,yrng(1)+ydel*0.02,std
xyouts,2730,yrng(1)+ydel*0.02,systime(0)
legend,['REF','REF+Nse','GALEX', $
	'REF(S/N)/1.E5','GALEX(<S/N>)/1.E5','S/N=10'], $
	linesty=[0,0,0,0,0,2],$
	thick=[th,1,1,th,th,1],charthi=th, charsi=li, $
	color=[colordex('red'),colordex('green'),!p.color,$
	colordex('A'),colordex('O'),0], box=0,/clear, $
	clr_color=!p.background, $
	pos=[2550,yrng(1)-ydel*0.03]
if keyword_set(ps) then $
	psclose $
else	read,'next: ',q
;
; plot ratios
;
; ps output
if keyword_set(ps) then begin
	psfile,'std_avrat',/color
	deepcolor
	!p.background=colordex('white')
	!p.color=colordex('black')
	!p.font=1
endif else deepcolor
cols = [colordex('P'),colordex('B')]	; FUV, NUV
plot,sw,rar,charsi=si,charthi=th,linesty=0,thick=th, $
	xtitle='WAVELENGTH (ANGSTROMS)',xran=[1250,3050], $
	xsty=1,xthick=th, $
	ytitle='<GALEX / REF FLUX>', yran=[-0.1,1.99],$
	ythick=th, ysty=1, title=strupcase(std),/nodata
oplot,[0,10000],[1,1],linesty=2,thick=2
oplot,[nwl(0),nwl(0)],[-100,100],thick=th,color=cols(1)
oplot,[nwl(1),nwl(1)],[-100,100],thick=th,color=cols(1)
oplot,[fwl(0),fwl(0)],[-100,100],thick=th,color=cols(0)
oplot,[fwl(1),fwl(1)],[-100,100],thick=th,color=cols(0)
oplot,sw,rar
xyouts,2250,0,'<NUV> = '+string(nmn*100.,form='(f5.1)')+' +- '+ $
	string(nsg*100.,form='(f5.1)')+' %',charsi=li,color=cols(1)
xyouts,1345,0,'<FUV> = '+string(fmn*100.,form='(f5.1)')+' +- '+ $
	string(fsg*100.,form='(f5.1)')+' %',charsi=li,color=cols(0)
legend,['<DET GRISM ANG> = '+string(dgar,form='(f5.1)'), $
	'EXPOSERE TIME = '+string(ext,form='(f7.1)'), $
	'<X OFF RMS> = '+string(xfr,form='(f5.2)'), $
	'<MEDN RESID> = '+string(mdr,form='(f5.2)'), $
	'NOBS = '+string(nav)], $
	box=0,/clear,clr_color=!p.background
xyouts,1250,2.01,std
xyouts,2730,2.01,systime(0)
if keyword_set(ps) then $
	psclose
free_lun,ol
;
!p.font=font_save
;
return
end
