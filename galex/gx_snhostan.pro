pro gx_snhostan,outmask=outmask
;
; Perform photometry and analysis on galex sub images.
;
common lowz_sne_info	; sndat structure
;
; get list of sn host galaxy images to work on
flist=file_search('*/*intbgsub.fits',count=nf)
if nf le 0 then begin
	print,'No files found.'
	return
endif
;
; open analysis log file
lfile='snhostan.log'
filestamp,lfile
openw,ol,lfile,/get_lun
printf,ol,'# SNHOSTAN run on '+systime(0)
printf,ol,'# host             mF    mFerr     mN    mNerr   SN       surv       expt       hx       hy  apr(px)  backg      file'
;
; loop over files
for i=0,nf-1 do begin
;
; header
    hdr = headfits(flist(i))
    band = sxpar(hdr,'band')
    srv = sxpar(hdr,'mpstype')
    subv= sxpar(hdr,'SUBVIS')
    nx = sxpar(hdr,'NAXIS1')
    ny = sxpar(hdr,'NAXIS2')
    nadd = sxpar(hdr,'NADDED')
    if nadd ge 1 then begin
    	phf = sxpar(hdr,'PHOF0001')
	sta=strsplit(phf,'/',/extract)
	tile = sta(5)
	mcfdir = '/home/galex/fltops/mains/01-vsn/'+tile+'/d/01-main/use/use/'
    endif else begin
    	phf = sxpar(hdr,'PHOFILE')
	sta=strsplit(phf,'/',/extract)
	tile = sta(5)
	mcfdir = strmid(phf,0,strpos(phf,'/',/reverse_search)+1)
    endelse
;
; sn name
    sta=strsplit(flist(i),'/',/extract)
    sn=sta(0)
;
; file name
    fn=sta(1)
;
; image base name
    base=strmid(fn,0,strpos(fn,'-'))
;
; image root
    tmp=fn
    rute=gettok(tmp,'.')
;
; is sn in sndat?
    p=snfind(sn)
    if p ge 0 then begin
	host = sndat(p).host
	snty = sndat(p).type
;
; background
	backg = 0.d0	; default to zero background
;
; analyze images
	im=mrdfits(sn+'/'+fn,0,hdr,/silent)
	expt=sxpar(hdr,'EXPTIME')
	im  = im - backg	; subtract background
	ima = im*expt+1000.	; create count image (with 1000 count bias)
;
; get aperture sizes in arcsec
	apr = [ sndat(p).hd25/1.5 ]	; D25 in arcsec / 1.5
;
; convert diameter to radius plus fudge
	apr = apr / 1.5
	napr=n_elements(apr)
;
; fix any bad apers
	bap=where(apr le 0.,nbap)
	if nbap gt 0 then apr(bap)=13.	; default minimum aperture
;
; sky radii
;	skrads=[apr(0)+1.,apr(0)+15.]
	skrads=[0.,0.]
;
; test for mcat
	mcfil = file_search(mcfdir+base+'-xd-mcat.fits', count=nmcf)
	mcfil = mcfil(0)
	if file_test(mcfil) then begin
	    mcat=mrdfits(mcfil,1,mchdr,/silent)
;
; foreground stars will have good NUV mags
	    good=where(mcat.nuv_mag gt 0., ngood)
	    if ngood gt 0 then begin
		cra  = mcat(good).alpha_j2000_merged
		cdec = mcat(good).delta_j2000_merged
		cs2n = mcat(good).nuv_flux/mcat(good).nuv_fluxerr
		cfuv = mcat(good).fuv_mag
		cnuv = mcat(good).nuv_mag
		nofuv= where(cfuv lt 0., nnf)
		if nnf gt 0 then $
			cfuv(nofuv) = 99.
		colr = cfuv - cnuv
		clas = mcat(good).nuv_class_star
		gcirc,2,sndat(p).hra,sndat(p).hdec,cra,cdec,dis
;
; now find objects within the aperture that have FUV - NUV > 1.0 and
; are stellar (clas > 0.8) and have reasonable S/N (> 3) and are not
; the nucleus of the galaxy itself (dis > 2.0 arcsec)
		near = where(dis le apr(0) and dis ge 9.0 and $
			     colr gt 1.0 and clas gt 0.8 and cs2n gt 3.0,nnr)
		if nnr gt 0 then begin
		    for j=0,nnr-1 do begin
;
; get pixel coords in image
			rastr = string(cra(near(j)), form='(f13.8)')
			decstr = string(cdec(near(j)), form='(f13.8)')
			cmd='sky2xy '+flist(i)+' '+rastr+' '+decstr
			spawn,cmd,res
			if strpos(res,'ff') lt 0 then begin
				stb=strsplit(res,/extract)
				cx=float(stb(4))
				cy=float(stb(5))
				dist_circle,dis,[nx,ny],cx,cy
;
; mask 4 pixels in radius
				mask=where(dis le 4.0, nmsk)
				if nmsk gt 0 then begin
					ima(mask) = 1000.
					print,'Masking at: ',cx,' ',cy,' ',sn
				endif	; nmsk gt 0
			endif	; strpos(res,'ff') lt 0
		    endfor	; j=0,nnr-1
;
; output mask file if requested
		    if keyword_set(outmask) then begin
		    	mfil=sn+'/'+rute+'_masked.fits'
		    	mwrfits,ima,mfil,hdr,/lscale,/silent
			print,'Wrote: ',mfil
		    endif
		endif	; nnr gt 0
	    endif 	; ngood gt 0
	endif else print,mcfil+' not found.'	; file_test(mcfil)
;
; get sn coords in image
	rastr = string(sndat(p).hra, form='(f13.8)')
	decstr = string(sndat(p).hdec, form='(f13.8)')
	cmd='sky2xy '+flist(i)+' '+rastr+' '+decstr
	spawn,cmd,res
;
; make sure we're not off the image
	if strpos(res,'ff') lt 0 then begin
		stb=strsplit(res,/extract)
		snx=float(stb(4))
		sny=float(stb(5))
		fzo = 6.18		; FUV zeropoint offset (25 - 18.82)
		nzo = 4.92		; NUV zeropoint offset (25 - 20.08)
		aper,ima,snx,sny,mg,mge,sky,skerr,1.,apr,skrads,[0.,0.], $
	    		/silent,/exact,setskyval=1000.
        	if band eq 1 then begin	; NUV
	    		mgn = mg - nzo + 2.5*alog10(expt)
	    		mgne = mge
	    		mgf = replicate(-99.,napr)
	    		mgfe = replicate(-9.,napr)
		endif else begin	; FUV
	    		mgf = mg - fzo + 2.5*alog10(expt)
	    		mgfe = mge
	    		mgn = replicate(-99.,napr)
	    		mgne = replicate(-9.,napr)
		endelse
;
; we are off the image
    	endif else begin
		snx=-9.
		sny=-9.
		mgf=replicate(-99.,napr)
		mgfe=replicate(-9.,napr)
		mgn=replicate(-99.,napr)
		mgne=replicate(-9.,napr)
    	endelse
;
; print out results
    	print,sn,host,snty,mgf(0),mgfe(0),mgn(0),mgne(0),srv,expt, $
		format='(a-10,a-15,a-10,4f7.2,2x,a-5,f9.1)'
    	printf,ol,host,mgf(0),mgfe(0),mgn(0),mgne(0),sn,srv,expt,snx,sny, $
		apr(0),backg,fn, $
		format='(a-15,4f8.3,2x,a-10,a-5,4f9.1,2x,e9.3,2x,a)'
    endif	; did we find sn in sndat?
endfor	; loop over files
;
; close files
free_lun,ol
;
return
end
