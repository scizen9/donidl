pro glga_get_data,apfile,mag,magerr,ra,dec,majax,minax,pa, $
	filter=filter, exptime=exptime, host=host, verbose=verbose
;+
; glga_get_data - get glga data
;-
pdir=!GLGA_ROOT
;
; Parse filename
fdecomp,apfile,disk,dir,name,qual
host   = gettok(name,'_')
filter = gettok(name,'_')
;

;
flist=file_search(pdir+'data/???D/photometry/*NUV_aperture.dat',count=nf)
if nf le 0 then begin
	print,'No *_aperture.dat files found, returning'
	return
endif else if keyword_set(verbose) then print,'Found this many: ',nf
;
if not keyword_set(local) then $
	ofil=!GALS_DATA + 'galex_apphot.dat' $
else	ofil='galex_apphot.dat'
filestamp,ofil,/arch
openw,ol,ofil,/get_lun
printf,ol,'# MAKETOTALPHOT - '+systime(0)
printf,ol,'#name                       fuv_majax  fuv_exptim     fuv_mag  fuv_magerr         fuv_npix   nuv_majax  nuv_exptim     nuv_mag  nuv_magerr         nuv_npix'
;
for i=0l,nf-1 do begin
	readcol,flist[i],na,nmg,nmerr,nint,int_e,nint_e_cnt,nint_e_bg, $
		nbkg,nnpix, format='f,f,f,f,f,f,f,f,f',/silent
	if not finite(nmg) then begin
		na=-99.99
		nmg=-99.99
		nmerr=-9.999
		nbkg=-99.99
		nnpix=0.
	endif
	p0=strpos(flist[i],'/',/reverse_search)+1
	nc=strpos(flist[i],'_')-p0
  	host=strmid(flist[i],p0,nc)
	p2=strpos(flist[i],'data')+4
	ddir=strmid(flist[i],p2,6)
	fbase=pdir+'data'+ddir+'uv/fits/'
	p3=strpos(flist[i],'NUV')
	fspec=flist[i]
	strput,fspec,'F',p3
;
; get exposure times
	hfl = fbase+host+'-nd-int.fits.gz'
	if file_exist(hfl) then begin
		hdr=headfits(hfl)
		next=sxpar(hdr,'EXPTIME')
	endif else next=-99.99
	hfl = fbase+host+'-fd-int.fits.gz'
	if file_exist(hfl) then begin
		hdr=headfits(hfl)
		fext=sxpar(hdr,'EXPTIME')
	endif else fext=-99.99
;
; get FUV photometry
      fuvf=file_search(fspec,count=n)
	if n ge 1 then begin
		readcol,fuvf[0],fa,fmg,fmerr,fint,int_e,fint_e_cnt,fint_e_bg, $
		fbkg,fnpix, format='f,f,f,f,f,f,f,f,f',/silent
		if not finite(fmg) then begin
			fa=-99.99
			fmg=-99.99
			fmerr=-9.999
			fbkg=-99.99
			fnpix=0.
		endif
	endif else begin
		fa=-99.99
		fmg=-99.99
		fmerr=-9.999
		fbkg=-99.99
		fnpix=0.
	endelse
	printf,ol,host,fa,fext,fmg,fmerr,fnpix,na,next,nmg,nmerr,nnpix, $
	      format='(a-25,f12.4,f12.2,2f12.4,e17.7,f12.4,f12.2,2f12.4,e17.7)'
        if keyword_set(verbose) then $
		print,string(13B),i+1,'/',nf,host,ddir, $
			format='($,a1,i5,a,i5,2x,a-26,2x,a)'
endfor
free_lun,ol
endif
;
; SDSS
;
if keyword_set(sdss) then begin
;
flist=file_search(pdir+'data/???D/photometry/*_r_aperture.dat',count=nf)
if nf le 0 then begin
	print,'No *_aperture.dat files found, returning'
	return
endif
;
if not keyword_set(local) then $
	ofil=!GALS_DATA + 'sdss_apphot.dat' $
else	ofil='sdss_apphot.dat'
filestamp,ofil,/arch
openw,ol,ofil,/get_lun
printf,ol,'# MAKETOTALPHOT - '+systime(0)
printf,ol,'#name                         majax       umag        umagerr     gmag        gmagerr     rmag        rmagerr     imag        imagerr     zmag        zmagerr'
;
for i=0l,nf-1 do begin
	readcol,flist[i],ra,rmg,rmerr, format='f,f,f',/silent
	if not finite(rmg) then begin
		ra=-99.99
		rmg=-99.99
		rmerr=-9.999
	endif
	p0=strpos(flist[i],'/',/reverse_search)+1
	nc=strpos(flist[i],'_')-p0
  	host=strmid(flist[i],p0,nc)
	p1=strpos(flist[i],'_r_')+1
	uspec=flist[i]
	strput,uspec,'u',p1
	gspec=flist[i]
	strput,gspec,'g',p1
	ispec=flist[i]
	strput,ispec,'i',p1
	zspec=flist[i]
	strput,zspec,'z',p1
	p2=strpos(flist[i],'data')+4
	ddir=strmid(flist[i],p2,6)
;
; get u photometry
      uf=file_search(uspec,count=n)
	if n ge 1 then begin
		readcol,uf[0],ua,umg,umerr, format='f,f,f',/silent
		if not finite(umg) then begin
			ua=-99.99
			umg=-99.99
			umerr=-9.999
		endif
	endif else begin
		ua=-99.99
		umg=-99.99
		umerr=-9.999
	endelse
;
; get g photometry
      gf=file_search(gspec,count=n)
	if n ge 1 then begin
		readcol,gf[0],ga,gmg,gmerr, format='f,f,f',/silent
		if not finite(gmg) then begin
			ga=-99.99
			gmg=-99.99
			gmerr=-9.999
		endif
	endif else begin
		ga=-99.99
		gmg=-99.99
		gmerr=-9.999
	endelse
;
; get i photometry
      iff=file_search(ispec,count=n)
	if n ge 1 then begin
		readcol,iff[0],ia,img,imerr, format='f,f,f',/silent
		if not finite(img) then begin
			ia=-99.99
			img=-99.99
			imerr=-9.999
		endif
	endif else begin
		ia=-99.99
		img=-99.99
		imerr=-9.999
	endelse
;
; get z photometry
      zf=file_search(zspec,count=n)
	if n ge 1 then begin
		readcol,zf[0],za,zmg,zmerr, format='f,f,f',/silent
		if not finite(zmg) then begin
			za=-99.99
			zmg=-99.99
			zmerr=-9.999
		endif
	endif else begin
		za=-99.99
		zmg=-99.99
		zmerr=-9.999
	endelse
;
; print it out
	printf,ol,host,ra,umg,umerr,gmg,gmerr,rmg,rmerr,img,imerr,zmg,zmerr, $
	      format='(a-25,f12.4,10f12.4)'
        if keyword_set(verbose) then $
		print,string(13B),i+1,'/',nf,host,ddir, $
			format='($,a1,i5,a,i5,2x,a-26,2x,a)'
endfor
free_lun,ol
;
endif
;
; 2MASS
;
if keyword_set(twomass) then begin
;
flist=file_search(pdir+'data/???D/photometry/*_j_aperture.dat',count=nf)
if nf le 0 then begin
	print,'No *_aperture.dat files found, returning'
	return
endif
;
if not keyword_set(local) then $
	ofil=!GALS_DATA + '2mass_apphot.dat' $
else	ofil='2mass_apphot.dat'
filestamp,ofil,/arch
openw,ol,ofil,/get_lun
printf,ol,'# MAKETOTALPHOT - '+systime(0)
printf,ol,'#name                         majax       jmag        jmagerr     hmag        hmagerr     kmag        kmagerr'
;
for i=0l,nf-1 do begin
	readcol,flist[i],ja,jmg,jmerr, format='f,f,f',/silent
	if not finite(jmg) then begin
		ja=-99.99
		jmg=-99.99
		jmerr=-9.999
	endif
	p0=strpos(flist[i],'/',/reverse_search)+1
	nc=strpos(flist[i],'_')-p0
  	host=strmid(flist[i],p0,nc)
	p1=strpos(flist[i],'_j_')+1
	hspec=flist[i]
	strput,hspec,'h',p1
	kspec=flist[i]
	strput,kspec,'k',p1
	p2=strpos(flist[i],'data')+4
	ddir=strmid(flist[i],p2,6)
;
; get h photometry
      hf=file_search(hspec,count=n)
	if n ge 1 then begin
		readcol,hf[0],ha,hmg,hmerr, format='f,f,f',/silent
		if not finite(hmg) then begin
			ha=-99.99
			hmg=-99.99
			hmerr=-9.999
		endif
	endif else begin
		ha=-99.99
		hmg=-99.99
		hmerr=-9.999
	endelse
;
; get k photometry
      kf=file_search(kspec,count=n)
	if n ge 1 then begin
		readcol,kf[0],ka,kmg,kmerr, format='f,f,f',/silent
		if not finite(kmg) then begin
			ka=-99.99
			kmg=-99.99
			kmerr=-9.999
		endif
	endif else begin
		ka=-99.99
		kmg=-99.99
		kmerr=-9.999
	endelse
;
; print it out
	printf,ol,host,ja,jmg,jmerr,hmg,hmerr,kmg,kmerr, $
	      format='(a-25,f12.4,6f12.4)'
        if keyword_set(verbose) then $
		print,string(13B),i+1,'/',nf,host,ddir, $
			format='($,a1,i5,a,i5,2x,a-26,2x,a)'
endfor
free_lun,ol
;
endif
;
; WISE
;
if keyword_set(wise) then begin
;
flist=file_search(pdir+'data/???D/photometry/*_w1_aperture.dat',count=nf)
if nf le 0 then begin
	print,'No *_aperture.dat files found, returning'
	return
endif
;
if not keyword_set(local) then $
	ofil=!GALS_DATA + 'wise_apphot.dat' $
else	ofil='wise_apphot.dat'
filestamp,ofil,/arch
openw,ol,ofil,/get_lun
printf,ol,'# MAKETOTALPHOT - '+systime(0)
printf,ol,'#name                         majax       w1mag       w1magerr    w2mag       w2magerr    w3mag       w3magerr    w4mag       w4magerr'
;
for i=0l,nf-1 do begin
	readcol,flist[i],w1a,w1mg,w1merr, format='f,f,f',/silent
	if not finite(w1mg) then begin
		w1a=-99.99
		w1mg=-99.99
		w1merr=-9.999
	endif
	p0=strpos(flist[i],'/',/reverse_search)+1
	nc=strpos(flist[i],'_')-p0
  	host=strmid(flist[i],p0,nc)
	p1=strpos(flist[i],'_w1_')+1
	w2spec=flist[i]
	strput,w2spec,'w2',p1
	w3spec=flist[i]
	strput,w3spec,'w3',p1
	w4spec=flist[i]
	strput,w4spec,'w4',p1
	p2=strpos(flist[i],'data')+4
	ddir=strmid(flist[i],p2,6)
;
; get w2 photometry
      w2f=file_search(w2spec,count=n)
	if n ge 1 then begin
		readcol,w2f[0],w2a,w2mg,w2merr, format='f,f,f',/silent
		if not finite(w2mg) then begin
			w2a=-99.99
			w2mg=-99.99
			w2merr=-9.999
		endif
	endif else begin
		w2a=-99.99
		w2mg=-99.99
		w2merr=-9.999
	endelse
;
; get w3 photometry
      w3f=file_search(w3spec,count=n)
	if n ge 1 then begin
		readcol,w3f[0],w3a,w3mg,w3merr, format='f,f,f',/silent
		if not finite(w3mg) then begin
			w3a=-99.99
			w3mg=-99.99
			w3merr=-9.999
		endif
	endif else begin
		w3a=-99.99
		w3mg=-99.99
		w3merr=-9.999
	endelse
;
; get w4 photometry
      w4f=file_search(w4spec,count=n)
	if n ge 1 then begin
		readcol,w4f[0],w4a,w4mg,w4merr, format='f,f,f',/silent
		if not finite(w4mg) then begin
			w4a=-99.99
			w4mg=-99.99
			w4merr=-9.999
		endif
	endif else begin
		w4a=-99.99
		w4mg=-99.99
		w4merr=-9.999
	endelse
;
; print it out
	printf,ol,host,w1a,w1mg,w1merr,w2mg,w2merr,w3mg,w3merr,w4mg,w4merr, $
	      format='(a-25,f12.4,8f12.4)'
        if keyword_set(verbose) then $
		print,string(13B),i+1,'/',nf,host,ddir, $
			format='($,a1,i5,a,i5,2x,a-26,2x,a)'
endfor
free_lun,ol
;
endif
;
;
if keyword_set(verbose) then print,' '
;
return
end
