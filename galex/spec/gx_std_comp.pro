pro gx_std_comp,ps=ps,image=image
;+
; GX_STD_COMP - plot both GALEX and Std REF fluxes
;
;	gx_std_comp,/ps,/image
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
	print,'GX_STD_COMP - Error: wrong number of *-std.txt files found: ',nf
	return
endif
readcol,stdf[0],std,sra,sdec,format='a,d,d',/silent,comment='#'
std=std[0] & sra=sra[0] & sdec=sdec[0]
;
; get path list
pathsf = file_search('*-paths.txt',count=nf)
if nf ne 1 then begin
	print,'GX_STD_COMP - Error: wrong number of path list files found: ',nf
	return
endif
paths=gx_read_paths(pathsf[0],npth=npth,base=base,pat=pat,try=try,vis=vist)
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
; open error file
openw,el,std+'-errs.txt',/get_lun
printf,el,'# Paths with missing data: '+systime(0)
;
; loop over paths
for k=0,npth-1 do begin
;
; get file list
    flist=file_search(paths[k]+'/*-xg-gsp.fits', count=nf)
    if nf le 0 then begin
	printf,el,paths[k]
    endif else begin
;
; get pattern
	if strlen(strtrim(pat[k],2)) gt 0 then $
		ptrn = pat[k] $
	else	ptrn = 'BASE'
;
; check subdir
	if not file_test(ptrn,/directory) then $
		file_mkdir,ptrn
;
; open log file
	logf=ptrn+'/'+base[k]
	if strlen(strtrim(pat[k],2)) gt 0 then $
		logf = logf + '_'+pat[k]
	logf=logf+'_comp.dat'
	openw,ol,logf,/get_lun,/append
	printf,ol,'#',ptrn,vist[k],base[k],paths[k], $
		form='(a1,1x,a-12,2x,a,2x,a,2x,a)'
	printf,ol,'#VIS  SV     EXP    SID    DGRA  XRMS  MDRS    <FUV>   sFUV  <NUV>   sNUV'
;
; loop over files
	for i=0,nf-1 do begin
		gx_read_gsp,flist(i),sid,f,fe,w,dga,exptime,xorms,yorms,mdres, $
			visit,subvis,ra=sra,dec=sdec
;
; set up arrays
		if i eq 0 then begin
			far = fltarr(nf,n_elements(f))
			fear= fltarr(nf,n_elements(f))
			rar = fltarr(nf,n_elements(f))
			rnar= fltarr(nf,n_elements(f))
			gs2n= fltarr(nf,n_elements(f))
			avgs2n= fltarr(n_elements(f))
			dgar= fltarr(nf)
			ext = fltarr(nf)
			xfr = fltarr(nf)
			mdr = fltarr(nf)
			sids= lonarr(nf)
			vis = intarr(nf)
			svis= intarr(nf)
		endif
;
; plug in arrays
		far(i,*)  = f(*)
		fear(i,*) = fe(*)
		rar(i,*)  = f(*)/sf(*)
		rnar(i,*) = f(*)/sfn(*)
		gs2n(i,*) = f(*)/fe(*)
		avgs2n	  = avgs2n + f/fe
		dgar(i)   = dga
		ext(i)    = exptime
		xfr(i)    = xorms
		mdr(i)    = mdres
		sids(i)   = sid
		vis[i]    = visit
		svis[i]   = subvis
		print,base[k],ptrn,vist[k],subvis,sid, $
			form='(a,2x,a,2x,a,2x,i02,2x,i)'
	endfor
;
; avg GALEX s2n
	avgs2n = avgs2n / float(nf)
;
; nominal wavelength ranges
	nwl = [1850,3000]
	fwl = [1300,1800]
	nuv = where(w gt nwl(0) and w lt nwl(1))
	fuv = where(w gt fwl(0) and w lt fwl(1))
;
; where are we above ref s2n?
	rfs2n = 10.
	ngo = where(avgs2n(nuv) gt rfs2n, nngo)
	if nngo le 1 then $
		ngo = where(s2n(nuv) gt rfs2n, nngo)
	fgo = where(avgs2n(fuv) gt rfs2n, nfgo)
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
; plot individual observations
;
; loop
	for i=0,nf-1 do begin
;
; get pri images
		if keyword_set(image) then begin
			prif = flist(i)
			strput,prif,'pri',strpos(prif,'gsp')
			strput,prif,'n',strpos(prif,'xg')
			if file_exist(prif) then begin
				gx_read_pri,prif,sids(i),nphim
				ims,nphim,nsky,nskysig
			endif else nphim=fltarr(1000,2)
			strput,prif,'f',strpos(prif,'ng')
			if file_exist(prif) then begin
				gx_read_pri,prif,sids(i),fphim
				ims,fphim,fsky,fskysig
			endif else fphim=fltarr(1000,2)
			pos = [0.13,0.3,0.98,0.92]
		endif else pos = [0.13,0.15,0.98,0.92]
;
; ps output
		if keyword_set(ps) then begin
			pfl=ptrn+'/'+base[k]
			if strlen(strtrim(pat[k],2)) gt 0 then $
				pfl = pfl + '_'+pat[k]
			pfl=pfl+'_'+string(vis[i],form='(i04)')
			if svis[i] gt 0 then $
			     pfl=pfl+'_'+string(svis[i],form='(i02)')+'_comp' $
			else pfl=pfl+'_comp'
			psfile,pfl,/color
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
			ytitle='FLUX (pho/sec/cm!U2!N/Ang)', yran=yrng,$
			ythick=th, ysty=1, title=strupcase(std), $
			position=pos
		oplot,sw,s2n/10000.,color=colordex('A'),thick=th
		oplot,sw,sfn,color=colordex('green')
		oplot,sw,sf,color=colordex('red'),thick=th
		oplot,w,far(i,*)
		oplot,w,gs2n(i,*)/10000.,color=colordex('O'),thick=th
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
; individual stats
		mystats,rar(i,nuv),nmn,nsg
		mystats,rar(i,fuv),fmn,fsg
		printf,ol,vis[i],svis[i],ext[i],sids[i],dgar[i],xfr[i],mdr[i], $
			fmn*100.,fsg*100.,nmn*100.,nsg*100., $
			format='(i04,2x,i02,f8.1,i7,2x,3f6.2,2x,4f7.1)'
		print,vis[i],svis[i],ext[i],sids[i],dgar[i],xfr[i],mdr[i], $
			fmn*100.,fsg*100.,nmn*100.,nsg*100., $
			format='(i04,2x,i02,f8.1,i7,2x,3f6.2,2x,4f7.1)'
		xyouts,2250,-0.001,'<NUV> = '+string(nmn*100.,form='(f5.1)')+ $
			' +- '+ string(nsg*100.,form='(f5.1)')+' %', $
			charsi=li,color=cols(1)
		xyouts,1345,-0.001,'<FUV> = '+string(fmn*100.,form='(f5.1)')+ $
			' +- '+ string(fsg*100.,form='(f5.1)')+' %', $
			charsi=li,color=cols(0)
		legend,['SPEC ID ='+strn(sids(i)), $
			'DET GRISM ANG = '+string(dgar(i),form='(f5.1)'), $
			'EXPOSERE TIME = '+string(ext(i),form='(f7.1)'), $
			'X OFF RMS = '+string(xfr(i),form='(f5.2)'), $
			'MEDN RESID = '+string(mdr(i),form='(f5.2)')], $
			box=0,/clear,clr_color=!p.background
		xyouts,1250,yrng(1)+ydel*0.02,try[k]+': '+base[k]+' '+pat[k]+' '+ $
			string(vis[i],form='(i04)')+' '+ $
			string(svis[i],form='(i02)')
		xyouts,2730,yrng(1)+ydel*0.02,systime(0)
		legend,['REF','REF+Nse','GALEX', $
			'REF(S/N)/1.E5','GALEX(S/N)/1.E5','S/N=10'], $
			linesty=[0,0,0,0,0,2],$
			thick=[th,1,1,th,th,1],charthi=th, charsi=li, $
			color=[colordex('red'),colordex('green'),!p.color,$
			colordex('A'),colordex('O'),0], box=0,/clear, $
			clr_color=!p.background, $
			pos=[2550,yrng(1)-ydel*0.03]
;
; plot image
;	xyouts,1700,yrng(0)+ydel*0.72,'NUV',charsi=li,charthi=th
;	xyouts,1700,yrng(0)+ydel*0.59,'FUV',charsi=li,charthi=th
		if keyword_set(image) then begin
	    	if keyword_set(ps) then begin
			loadct,0
			tvscl,nphim(100:450,*)>0<imhi,6.2,0.27, $
				xsize=3.15,ysize=0.75,/inches
			tvscl,fphim(200:740,*)>0<imhi,1.2,0.27, $
				xsize=4.5,ysize=0.75,/inches
	    	endif else begin
			tvscl,nphim(100:450,*)>0<imhi,2400,yrng(0)-ydel*0.30,/data
			tvscl,fphim(200:740,*)>0<imhi,1250,yrng(0)-ydel*0.30,/data
	    	endelse
		endif
		if keyword_set(ps) then $
			psclose $
		else	read,'next: ',q
	endfor
;
; plot group observations
;
; ps output
	if keyword_set(ps) then begin
		pfl=ptrn+'/'+base[k]
		if strlen(strtrim(pat[k],2)) gt 0 then $
			pfl = pfl + '_'+pat[k]
		pfl=pfl+'_'+string(vis[0],form='(i04)')+'_gratio'
		psfile,pfl,/color
		deepcolor
		!p.background=colordex('white')
		!p.color=colordex('black')
		!p.font=1
	endif else deepcolor
	cols = [colordex('P'),colordex('B')]	; FUV, NUV
;
; loop
	for i=0,nf-1 do begin
;
		if i eq 0 then begin
		    plot,sw,rar(i,*),charsi=si,charthi=th,linesty=0,thick=th, $
			xtitle='WAVELENGTH (ANGSTROMS)',xran=[1250,3050], $
			xsty=1,xthick=th, $
			ytitle='GALEX / REF FLUX', yran=[-0.1,1.99],$
			ythick=th, ysty=1, title=strupcase(std)
		    oplot,[0,10000],[1,1],linesty=2,thick=2
		    oplot,[nwl(0),nwl(0)],[-100,100],thick=th,color=cols(1)
		    oplot,[nwl(1),nwl(1)],[-100,100],thick=th,color=cols(1)
		    oplot,[fwl(0),fwl(0)],[-100,100],thick=th,color=cols(0)
		    oplot,[fwl(1),fwl(1)],[-100,100],thick=th,color=cols(0)
		endif else $
		    oplot,sw,rar(i,*)
	endfor
;
; group stats
	mystats,rar(*,nuv),nmn,nsg
	mystats,rar(*,fuv),fmn,fsg
	if nf gt 1 then begin
	    printf,ol,vis[0],0,avg(ext),sids[0],avg(dgar),avg(xfr),avg(mdr), $
		fmn*100.,fsg*100.,nmn*100.,nsg*100., $
		format='(i04,2x,i02,f8.1,i7,2x,3f6.2,2x,4f7.1)'
	    print,vis[0],0,avg(ext),sids[0],avg(dgar),avg(xfr),avg(mdr), $
		fmn*100.,fsg*100.,nmn*100.,nsg*100., $
		format='(i04,2x,i02,f8.1,i7,2x,3f6.2,2x,4f7.1)'
	endif
	xyouts,2250,0,'<NUV> = '+string(nmn*100.,form='(f5.1)')+' +- '+ $
		string(nsg*100.,form='(f5.1)')+' %',charsi=li,color=cols(1)
	xyouts,1345,0,'<FUV> = '+string(fmn*100.,form='(f5.1)')+' +- '+ $
		string(fsg*100.,form='(f5.1)')+' %',charsi=li,color=cols(0)
	legend,['SPEC ID ='+strn(sids(0)), $
		'DET GRISM ANG = '+string(avg(dgar),form='(f5.1)'), $
		'EXPOSERE TIME = '+string(avg(ext),form='(f7.1)'), $
		'X OFF RMS = '+string(avg(xfr),form='(f5.2)'), $
		'MEDN RESID = '+string(avg(mdr),form='(f5.2)'), $
		'NOBS = '+string(nf)], $
		box=0,/clear,clr_color=!p.background
	xyouts,1250,2.01,try[k]+': '+base[k]+' '+pat[k]+' '+ $
		string(vis[0],form='(i04)')
	xyouts,2730,2.01,systime(0)
	if keyword_set(ps) then $
		psclose $
	else	read,'next: ',q
	free_lun,ol
    endelse
endfor	; loop over paths
;
!p.font=font_save
;
return
end
